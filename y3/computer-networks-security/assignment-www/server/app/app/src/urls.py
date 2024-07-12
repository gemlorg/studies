from django.contrib import admin
from django.urls import include, path

urlpatterns = [
    path("", include("src.apps.greeting_card.urls", namespace="greeting-card")),
    path("accounts/", include("src.apps.accounts.urls")),
    path("accounts/", include("django.contrib.auth.urls")),
]
