FROM public.ecr.aws/lambda/python:3.10-arm64

ARG FUNCTION_NAME

COPY requirements.txt ${LAMBDA_TASK_ROOT}

COPY pkg ${LAMBDA_TASK_ROOT}/pkg

COPY cmd/lambda/${FUNCTION_NAME}/main.py ${LAMBDA_TASK_ROOT}

RUN if [ "$FUNCTION_NAME" = "createpositions" ]; then cp lstm_model.h5 ${LAMBDA_TASK_ROOT}/ ; fi

RUN yum update -y && yum install -y libxml2-devel libxslt-devel gcc

RUN pip3 install --requirement requirements.txt --target "${LAMBDA_TASK_ROOT}"

ENV S3_DATA_BUCKET_NAME=""

ENV ALPACA_API_KEY=""

ENV ALPACA_API_SECRET=""

ENV DARQUBE_API_KEY=""

ENV ALPHA_VANTAGE_API_KEY=""

ENV MODEL_FILE_PATH=""

ENV IS_PAPER=""

ENV SNS_ERRORS_TOPIC_ARN=""

CMD [ "main.handler" ]
