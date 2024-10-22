# Generated by Django 4.2 on 2023-05-08 23:37

from django.db import migrations, models
import django.db.models.deletion


class Migration(migrations.Migration):

    dependencies = [
        ('polls', '0009_alter_section_status_alter_section_status_data'),
    ]

    operations = [
        migrations.AlterField(
            model_name='section',
            name='kind',
            field=models.ForeignKey(blank=True, null=True, on_delete=django.db.models.deletion.CASCADE, to='polls.sectionkind'),
        ),
        migrations.AlterField(
            model_name='section',
            name='name',
            field=models.CharField(blank=True, max_length=40),
        ),
        migrations.AlterField(
            model_name='section',
            name='status',
            field=models.ForeignKey(blank=True, null=True, on_delete=django.db.models.deletion.CASCADE, to='polls.sectionstatus'),
        ),
        migrations.AlterField(
            model_name='section',
            name='status_data',
            field=models.ForeignKey(blank=True, null=True, on_delete=django.db.models.deletion.CASCADE, to='polls.statusdata'),
        ),
    ]
