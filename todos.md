# Consider validation for required configuration fields.

While defaulting to empty strings provides type consistency, it may mask configuration errors. Consider adding validation to ensure critical fields like API keys and project identifiers are properly configured.

Also note the inconsistency: credentials_path on line 24 still uses the old pattern without an empty string default.

```diff
 class GCP(BaseModel):
     bucket: Bucket = Bucket()
-    credentials_path: str = os.getenv("GOOGLE_APPLICATION_CREDENTIALS", "")
+    credentials_path: str = Field(default=os.getenv("GOOGLE_APPLICATION_CREDENTIALS", ""))
```
Consider adding validation to ensure required fields are non-empty:

```python
from pydantic import BaseModel, Field, field_validator

class Polygon(BaseModel):
    api_key: str = Field(default=os.getenv("POLYGON_API_KEY", ""))
    
    @field_validator('api_key')
    @classmethod
    def validate_api_key(cls, v):
        if not v:
            raise ValueError('POLYGON_API_KEY environment variable is required')
        return v
```

In application/datamanager/src/datamanager/config.py at line 8 and also lines
14-15 and 24, the current use of default empty strings for critical config
fields like api_key and credentials_path can hide missing configuration errors.
To fix this, add pydantic field validators for these fields that check if the
value is non-empty and raise a ValueError if not. This ensures required
environment variables are properly set and prevents silent misconfiguration.

# Move index computation after data transformations.

The column indices are captured too early, before the data undergoes significant transformations (joins, new columns, reordering). This causes the indices to become stale.

Move the index computation after the data is finalized:

```diff
-        self.preprocessors["indices"] = {
-            col: idx for idx, col in enumerate(data.columns)
-        }
-
         data = data.unique(subset=["ticker", "timestamp"])
         # ... all transformations ...
         data = data.sort(["ticker", "timestamp"])
+        
+        # Compute indices after all transformations
+        self.preprocessors["indices"] = {
+            col: idx for idx, col in enumerate(data.columns)
+        }
```

Also applies to: 84-85

In application/predictionengine/src/predictionengine/dataset.py at lines 48-50
and also lines 84-85, the computation of column indices is done too early before
the data transformations like joins, adding new columns, or reordering. This
causes the stored indices to be outdated. To fix this, move the code that sets
self.preprocessors["indices"] to after all data transformations are complete,
ensuring the indices reflect the final state of the data columns.

# Fix incorrect scalers check and remove redundant type annotation.

The condition self.scalers is None will never be true since self.scalers is initialized as {} in __init__. Also, re-declaring the instance variable with a type annotation is unnecessary and confusing.

```diff
-        if self.scalers is None or len(self.scalers) == 0:
-            self.scalers: Dict[str, Dict[str, Tensor]] = {}
+        if len(self.scalers) == 0:
```

In application/predictionengine/src/predictionengine/dataset.py around lines 117
to 118, remove the check for `self.scalers is None` since `self.scalers` is
always initialized as an empty dictionary in `__init__`. Also, eliminate the
redundant type annotation on `self.scalers` when assigning it to an empty
dictionary to avoid confusion and maintain clarity.

# Fix inconsistent None check for required parameter.

The parameter output_size is declared as int (required), but line 12 checks if it's None. This is inconsistent since a required int parameter cannot be None.

```diff
-    output_size: int,
+    output_size: int = None,
```

Or if output_size should always be provided, remove the None check:
```
-    output_size = output_size if output_size is not None else input_size
+    # output_size is always provided as per function signature
```

In application/predictionengine/src/predictionengine/gated_residual_network.py
at line 12, the code checks if the required int parameter output_size is None,
which is inconsistent. To fix this, either remove the None check entirely if
output_size must always be provided, or update the function signature to make
output_size optional by setting a default value of None if the None check is
necessary.

# Simplify the batch iteration logic.
Since the dataset is configured with batch_size=1 and sample_count=1, there will only be one batch. The nested loop structure is unnecessary and confusing.


```diff
-            for tickers_batch, features_batch, _ in dataset.batches():
-                percentile_25, percentile_50, percentile_75 = model.predict(
-                    tickers_batch, features_batch
-                )
-
-                predictions[ticker] = {
-                    "percentile_25": float(percentile_25[0]),
-                    "percentile_50": float(percentile_50[0]),
-                    "percentile_75": float(percentile_75[0]),
-                }
-                break
+            # Get the single batch
+            batch_generator = dataset.batches()
+            tickers_batch, features_batch, _ = next(batch_generator)
+            
+            percentile_25, percentile_50, percentile_75 = model.predict(
+                tickers_batch, features_batch
+            )
+
+            predictions[ticker] = {
+                "percentile_25": float(percentile_25[0]),
+                "percentile_50": float(percentile_50[0]),
+                "percentile_75": float(percentile_75[0]),
+            }
```

In application/predictionengine/src/predictionengine/main.py around lines 136 to
147, the code uses a for loop to iterate over dataset batches, but since
batch_size and sample_count are both 1, there is only one batch. Replace the for
loop with a direct call to get the single batch from the dataset, then perform
the prediction and assignment without looping, simplifying the logic and
improving readability.

# Quantile loss shape handling is correct; add value-based assertions

The quantile_loss implementation uses broadcasting (error = y_true - y_pred) and .mean() to reduce to a scalar (0-D Tensor) or a single-element Tensor, so the existing shape assertions (() or (1,)) are valid.
To improve test robustness, add explicit assertions on the numeric value of the loss for known inputs. For example, compute the expected loss by hand for a small input and confirm quantile_loss matches it.
Consider covering additional edge cases:
Perfect prediction (loss == 0)
Negative vs positive errors
Single-quantile scenarios

In application/predictionengine/tests/test_loss_function.py around lines 8 to
17, the test currently only checks the type and shape of the quantile_loss
output but does not verify its numeric correctness. Enhance the test by adding
explicit assertions that compare the computed loss value against expected values
calculated manually for the given inputs. Also, add additional test cases
covering perfect predictions (where loss should be zero), scenarios with
negative and positive errors, and tests with a single quantile to ensure
comprehensive coverage of quantile_loss behavior.

# Fix shape mismatch between LSTM output and self-attention input

The LSTM encoder’s forward returns only the last hidden state of shape (batch_size, hidden_size), but MultiHeadSelfAttention.forward unpacks its input as

batch_size, sequence_length, _ = input.shape
which requires a 3-D tensor (batch_size, seq_len, embedding_size).

You must update one of the following:

Have the LSTM return the full sequence of hidden states (e.g. collect layer_hidden_state at each time step) so that lstm_output is (batch_size, seq_len, hidden_size) before passing through the GRN and self-attention layers.
Or replace/adjust the self-attention module (or use a different attention mechanism) that accepts a 2-D input of shape (batch_size, hidden_size).
Locations to address:

application/predictionengine/src/predictionengine/miniature_temporal_fusion_transformer.py
lines 86–90 (the call to self.self_attention.forward(processed_features))

In
application/predictionengine/src/predictionengine/miniature_temporal_fusion_transformer.py
around lines 84 to 90, the LSTM encoder's forward method returns only the last
hidden state with shape (batch_size, hidden_size), but the self-attention layer
expects input of shape (batch_size, sequence_length, embedding_size). To fix
this, modify the LSTM encoder to return the full sequence of hidden states so
that lstm_output has shape (batch_size, seq_len, hidden_size), or alternatively
replace or adjust the self-attention layer to accept a 2-D input of shape
(batch_size, hidden_size). Ensure that the processed_features passed to
self.self_attention.forward have the correct 3-D shape if keeping the
self-attention layer.


# Fix the incorrect batch iteration logic and typo.

The nested loop structure is incorrect. dataset.batches() returns individual batches, not a collection to iterate over. Also, there's a typo in the variable name and the loss is being appended in the wrong place.

```diff
-            for batch in dataset.batches():
-                for tickers, historical_features, targets in batch:
-                    predictions, _, _ = self.forward(
-                        Tensor(tickers),
-                        Tensor(historical_features),
-                    )
-
-                    loss = quantile_loss(predictions, Tensor(targets), quantiles)
-
-                    optimizer.zero_grad()
-                    loss.backward()
-                    optimizer.step()
-
-                    epoch_loss += loss.numpy().item()
-
-                avgerage_epoch_loss = epoch_loss / len(dataset)
-                losses.append(avgerage_epoch_loss)
+            batch_count = 0
+            for tickers, historical_features, targets in dataset.batches():
+                predictions, _, _ = self.forward(
+                    Tensor(tickers),
+                    Tensor(historical_features),
+                )
+
+                loss = quantile_loss(predictions, Tensor(targets), quantiles)
+
+                optimizer.zero_grad()
+                loss.backward()
+                optimizer.step()
+
+                epoch_loss += loss.numpy().item()
+                batch_count += 1
+            
+            average_epoch_loss = epoch_loss / batch_count
+            losses.append(average_epoch_loss)
```

# Fix the batch iteration in validation method.

The batch iteration has the same issue as in the train method. Also, len(dataset) might not return the correct number of batches.

```diff
     def validate(
         self,
         dataset: DataSet,
     ) -> float:
         total_loss = 0.0
-        batch_count = len(dataset)
+        batch_count = 0
 
-        for batch in dataset.batches():
-            tickers, features, targets = batch
+        for tickers, features, targets in dataset.batches():
             tickers, features, targets = (
                 Tensor(tickers),
                 Tensor(features),
                 Tensor(targets),
             )
+            batch_count += 1
```

In
application/predictionengine/src/predictionengine/miniature_temporal_fusion_transformer.py
around lines 142 to 150, the batch iteration in the validation method
incorrectly uses len(dataset) for batch count and iterates over
dataset.batches() similarly to the train method, which is problematic. Fix this
by using the correct method or property to get the number of batches and ensure
the batch iteration matches the correct approach used in the train method,
properly unpacking and converting batch elements to Tensors.
