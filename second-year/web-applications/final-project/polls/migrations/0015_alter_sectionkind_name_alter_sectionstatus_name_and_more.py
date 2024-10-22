# Generated by Django 4.2 on 2023-05-23 23:03

from django.conf import settings
from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        migrations.swappable_dependency(settings.AUTH_USER_MODEL),
        ('polls', '0014_alter_catalog_owner_alter_file_owner_delete_user'),
    ]

    operations = [
        migrations.AlterField(
            model_name='sectionkind',
            name='name',
            field=models.CharField(max_length=40, unique=True),
        ),
        migrations.AlterField(
            model_name='sectionstatus',
            name='name',
            field=models.CharField(max_length=40, unique=True),
        ),
        migrations.AlterUniqueTogether(
            name='catalog',
            unique_together={('name', 'owner', 'parent_dir')},
        ),
        migrations.AlterUniqueTogether(
            name='file',
            unique_together={('name', 'owner', 'parent_dir')},
        ),
        migrations.AlterUniqueTogether(
            name='section',
            unique_together={('start', 'file')},
        ),
        migrations.AlterUniqueTogether(
            name='statusdata',
            unique_together={('name', 'data')},
        ),
    ]
