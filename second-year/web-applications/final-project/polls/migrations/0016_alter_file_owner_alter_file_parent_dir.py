# Generated by Django 4.2 on 2023-05-23 23:16

from django.conf import settings
from django.db import migrations, models
import django.db.models.deletion


class Migration(migrations.Migration):

    dependencies = [
        migrations.swappable_dependency(settings.AUTH_USER_MODEL),
        ('polls', '0015_alter_sectionkind_name_alter_sectionstatus_name_and_more'),
    ]

    operations = [
        migrations.AlterField(
            model_name='file',
            name='owner',
            field=models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, to=settings.AUTH_USER_MODEL),
        ),
        migrations.AlterField(
            model_name='file',
            name='parent_dir',
            field=models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, to='polls.catalog'),
        ),
    ]