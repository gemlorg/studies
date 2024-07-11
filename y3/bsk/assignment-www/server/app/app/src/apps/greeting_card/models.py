import binascii
import os

from django.contrib.auth.models import User
from django.db import models
from django.db.models import SET_NULL
from django.urls import reverse


class GreetingCard(models.Model):
    token = models.CharField(max_length=255)
    recipient = models.TextField()
    sender = models.ForeignKey(User, on_delete=SET_NULL, null=True)
    content = models.TextField()
    template = models.TextField()
    read = models.BooleanField(default=False)

    def update_token_if_needed(self):
        if not self.token:
            self.token = binascii.hexlify(os.urandom(32)).decode("ascii")

    def get_absolute_url(self):
        return reverse("greeting-card:show", args=[self.token])

    def save(self):
        self.update_token_if_needed()
        super().save()
