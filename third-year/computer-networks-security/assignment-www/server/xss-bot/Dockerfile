FROM tasks-base-python:latest

RUN apt-get update -y && apt-get install -y wget zip
RUN wget -q -O - --no-check-certificate https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add -
RUN echo "deb http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list
RUN apt-get update -y  && \
    apt-get install -y  google-chrome-stable
RUN wget https://edgedl.me.gvt1.com/edgedl/chrome/chrome-for-testing/118.0.5993.70/linux64/chromedriver-linux64.zip \
     && unzip chromedriver-linux64.zip \
     && mv chromedriver-linux64/chromedriver /usr/bin
COPY requirements.txt /tmp/requirements.txt
RUN pip3 --no-cache-dir install --break-system-packages -r /tmp/requirements.txt

COPY . /app
CMD ["python3", "-u", "/app/bot.py"]
