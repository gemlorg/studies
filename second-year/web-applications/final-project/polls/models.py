import datetime

from django.db import models
from django.utils import timezone
from django.contrib.auth.models import User



class Catalog(models.Model):
    name = models.CharField(max_length=40, blank=False, unique=True, null=False)
    description = models.CharField(max_length=200, blank=True)
    add_date =  models.DateTimeField('date added' , blank=True , auto_now_add=True, null=True)
    owner = models.ForeignKey(User , on_delete=models.CASCADE)
    is_available = models.BooleanField(default=True)
    available_date = models.DateTimeField('date modified access', blank=True , auto_now_add=True, null=True)
    edit_date = models.DateTimeField('date modified', blank=True , auto_now_add=True, null=True)
    parent_dir = models.ForeignKey('self', on_delete=models.CASCADE, blank=True, null=True)
    
    def save(self, *args, **kwargs):
        if(self.name == ""):
            raise Exception
        super(Catalog, self).save(*args, **kwargs)

    def __str__(self):
        return self.name
    class Meta:
        unique_together = (("name", "owner", "parent_dir"),)

class File(models.Model):
    name =  models.CharField(max_length=40, blank=False, unique=True, null=False)
    description = models.CharField(max_length=200, blank=True)
    add_date =  models.DateTimeField('date added' , blank=True , auto_now_add=True, null=True)
    owner = models.ForeignKey(User, on_delete=models.CASCADE)
    is_available = models.BooleanField(default=True)
    available_date = models.DateTimeField('date modified access', blank=True , auto_now_add=True, null=True)
    edit_date = models.DateTimeField('date modified', blank=True , auto_now_add=True, null=True)
    parent_dir = models.ForeignKey(Catalog, on_delete=models.CASCADE, blank=False)

    def save(self, *args, **kwargs):
        if(self.name == ""):
            raise Exception
        super(File, self).save(*args, **kwargs)


    def __str__(self):
        return self.name
    
    class Meta:
        unique_together = (("name", "owner", "parent_dir"),)


class SectionKind(models.Model):
    name = models.CharField(max_length=40, unique=True, blank=False)


    def __str__(self):
        return self.name

class SectionStatus(models.Model):
    name = models.CharField(max_length=40, unique=True, blank=False)


    def __str__(self):
        return self.name

class StatusData(models.Model):
    name = models.CharField(max_length=40) 
    data = models.CharField(max_length=400) 

    def __str__(self):
        return self.name
    class Meta:
        unique_together = (("name", "data"),)

class Section(models.Model):
    name =  models.CharField(max_length=40, blank=True) 
    description = models.CharField(max_length=200, blank=True)
    add_date =  models.DateTimeField('date added', blank=True , auto_now_add=True, null=True)
    start = models.IntegerField()
    end = models.IntegerField()
    kind = models.ForeignKey(SectionKind, on_delete=models.CASCADE, blank=True, null=True)
    status = models.ForeignKey(SectionStatus, on_delete=models.CASCADE, blank=True, null=True)
    status_data = models.ForeignKey(StatusData, on_delete=models.CASCADE, blank=True, null=True)
    content = models.JSONField(max_length=1000, blank=True)
    file = models.ForeignKey(File, on_delete=models.CASCADE, null=True)
    def __str__(self):
        return self.name
    def save(self, *args, **kwargs):
        if(self.start < 0 or self.end < self.start):
            raise Exception
        return super(Section, self).save(*args, **kwargs)
    class Meta:
        unique_together = (("start", "file"),)

    
    
