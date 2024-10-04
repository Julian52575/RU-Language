FROM python:3

COPY . .

RUN apt-get update && apt-get install chezscheme

CMD ["python3", "compareScheme.py", "examples"]
