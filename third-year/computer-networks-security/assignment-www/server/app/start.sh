cd /home/app/task/
python3 manage.py migrate
python3 manage.py loaddata --format=json - < fixture
rm fixture
echo FLAG{[redacted]} > /flag.txt
uwsgi --ini /home/app/app.ini --die-on-term
