from django import forms
from django.contrib.auth.forms import UserCreationForm
from django.contrib.auth.models import User

from .models import Profile


class RegisterForm(UserCreationForm):
    footer = forms.TextInput()

    class Meta:
        model = User
        fields = ["username", "email", "password1", "password2"]
        error_messages = {
            "username": {
                "unique": "Użytkownik o podanej nazwie już istnieje.",
            }
        }

    error_messages = {
        "password_mismatch": "Podane hasła się nie zgadzają.",
    }

    def save(self, commit=True):
        footer = self.data["footer"]
        # Save user first
        user = super().save(commit=True)
        profile = Profile.objects.create(user=user)
        profile.footer = footer
        profile.save()
        return user
