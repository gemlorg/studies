import subprocess
import tempfile

from django.contrib.auth.mixins import LoginRequiredMixin
from django.http import HttpResponse
from django.views import generic
from src.apps.greeting_card.models import GreetingCard

from .utils import render_card


class GreetingCardListView(LoginRequiredMixin, generic.ListView):
    template_name = "greeting_card/list.html"
    model = GreetingCard

    def get_queryset(self, *args, **kwargs):
        objects = GreetingCard.objects.filter(recipient=self.request.user.username)
        if self.request.GET.get("unread", None) is not None:
            objects = objects.filter(read=False)
        return objects

    def get_context_data(self, **kwargs):
        context = super(GreetingCardListView, self).get_context_data(**kwargs)
        context["all"] = self.request.GET.get("unread", None) is None
        return context


class GreetingCardCreateView(LoginRequiredMixin, generic.CreateView):
    template_name = "greeting_card/create.html"
    model = GreetingCard
    fields = ["recipient", "content", "template"]

    def form_valid(self, form):
        form.instance.sender = self.request.user
        return super().form_valid(form)


class GreetingCardDetailView(LoginRequiredMixin, generic.DetailView):
    template_name = "greeting_card/detail.html"
    model = GreetingCard

    slug_url_kwarg = "token"
    slug_field = "token"

    def get(self, request, *args, **kwargs):
        response = super().get(request, *args, **kwargs)
        if self.request.user.username == self.object.recipient:
            self.object.read = True
        self.object.save()
        return response

    def get_context_data(self, **kwargs):
        context = super(GreetingCardDetailView, self).get_context_data(**kwargs)
        rendered = render_card(self.object)
        context["content"] = rendered
        return context


class GreetingCardDetailForPNGView(generic.DetailView):
    template_name = "greeting_card/detail_for_png.html"
    model = GreetingCard

    slug_url_kwarg = "token"
    slug_field = "token"

    def get_context_data(self, **kwargs):
        context = super(GreetingCardDetailForPNGView, self).get_context_data(**kwargs)
        rendered = render_card(self.object)
        context["content"] = rendered
        return context


class GreetingCardDownloadPNGView(LoginRequiredMixin, generic.View):
    model = GreetingCard

    def get(self, request, token):
        file_name = tempfile.mktemp(suffix=".png")
        subprocess.call(
            [
                "chromium",
                "--headless",
                "--disable-gpu",
                "--no-sandbox",
                "--screenshot=" + file_name,
                "http://127.0.0.1:3000/show-for-png/" + token,
            ]
        )
        with open(file_name, "rb") as f:
            return HttpResponse(f.read(), content_type="image/png")
