from django.urls import include, path
from django.views.decorators.csrf import csrf_exempt
from src.apps.greeting_card import views

app_name = "greeting_card"

urlpatterns = [
    path("", views.GreetingCardListView.as_view()),
    path("create", views.GreetingCardCreateView.as_view()),
    path("show/<str:token>", views.GreetingCardDetailView.as_view(), name="show"),
    path(
        "show-for-png/<str:token>",
        views.GreetingCardDetailForPNGView.as_view(),
        name="show-for-png",
    ),
    path(
        "download-png/<str:token>",
        views.GreetingCardDownloadPNGView.as_view(),
        name="download-png",
    ),
]
