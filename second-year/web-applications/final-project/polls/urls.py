from django.urls import path, include

from django.contrib import admin
from . import views

app_name = 'polls'
urlpatterns = [
    path('', views.IndexView.as_view(), name='index'),
    path('get_file_text', views.get_file_text , name='get_file_text'),
    path('get_compile_file', views.get_compile_file, name='get_compile_file'),
     path('file_receiver', views.file_receiver, name='file_receiver'),
     path('delete_folder', views.delete_folder, name='delete_folder'),
     path('delete_file', views.delete_file, name='delete_file'),
     path('new_folder/', views.new_folder, name='new_folder'),
     path('compile_options', views.compile_options, name='compile_options'),
     path('login', views.LoginView.as_view(), name="login"),
     path('do_login', views.do_login, name="do_login"),
    path('accounts/', include('django.contrib.auth.urls')),
]
