//! What a trading session journals of its own standing: why it halted and how it ended.

use serde::{Deserialize, Serialize};

use crate::common::order::{BrokerFailure, ClientOrderId};
use crate::common::time::SessionDate;

/// Why a session stopped deciding.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum HaltCause {
    /// The journal refused a write; the halt is still journaled, best effort.
    JournalRefused { error: String },
    /// The broker's book could not be read to reconcile against.
    ReconcileUnread(BrokerFailure),
    /// The broker's book diverged from the journal's; the `book_reconciled` record says where.
    Diverged,
    /// An order was left unresolved and may still be open at the broker.
    Unresolved { client_order_id: ClientOrderId },
}

/// The first halt of a session, journaled once with its cause.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SessionHalted {
    cause: HaltCause,
}

impl SessionHalted {
    pub fn new(cause: HaltCause) -> Self {
        Self { cause }
    }
}

/// How a trading session ended.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SessionEnding {
    NoSession,
    /// Ran to the close and the account held nothing there.
    ToTheClose,
    /// Ran to the close but the account still held `positions` positions.
    HeldAtTheClose {
        positions: usize,
    },
    /// Halted before the close, its cause journaled as `session_halted` when the journal took it.
    Halted,
    /// Refused before the open.
    StoppedBeforeTheOpen,
    /// Stopped by a failed step while trading.
    StoppedTrading,
}

/// The session a trader run was for and how the run ended, journaled whether or not `session_opened` was.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct SessionClosed {
    session: SessionDate,
    ending: SessionEnding,
}

impl SessionClosed {
    pub fn new(session: SessionDate, ending: SessionEnding) -> Self {
        Self { session, ending }
    }
}
