FROM tasks-base-python:latest

RUN apt-get -y install git uwsgi libpq-dev chromium

COPY wait-for-it.sh /wait-for-it.sh
COPY start.sh /start.sh
RUN chmod +x /wait-for-it.sh
RUN chmod +x /start.sh

COPY app/requirements.txt /requirements.txt
RUN pip3 install --break-system-packages -r /requirements.txt

COPY app.ini /home/app/
COPY app/ /home/app/task/

RUN chmod -R 755 /home/app

CMD ["bash", "-c", "/wait-for-it.sh zad41-mimuw-finals-2023-db:5432 -- /start.sh"]
