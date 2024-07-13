import json
import re
import subprocess

from . import functions
from django.http import HttpResponseRedirect, JsonResponse
from django.shortcuts import get_object_or_404, render
from django.urls import reverse
from django.views import generic
from django.utils import timezone
from django.views.generic.base import TemplateView
from django.core import serializers
from django.forms.models import model_to_dict
from django.utils import timezone
from django.shortcuts import redirect
from django.contrib.auth.decorators import login_required

from django.db.models import Count

from .models import Catalog, File, Section, SectionKind

from django.contrib.auth.models import User


global standard
global optimisations
global processor
global dependencies

standard = "c11"
optimisations = []
processor = "mcs51"
dependencies = []


class IndexView(TemplateView):
    template_name = 'polls/index.html'


    def get_context_data(self, **kwargs):
        context = super().get_context_data(**kwargs)
        context['files'] = File.objects.filter(is_available=True)
        context['catalogs'] = Catalog.objects.filter(is_available=True)
        context['tree'] = get_tree(self.request)
        return context

class LoginView(TemplateView):
    template_name = 'polls/login.html'

    def get_context_data(self, **kwargs):
        context = super().get_context_data(**kwargs)
        return context

def do_login(request):
    return redirect('/polls/')

@login_required
def get_file_text(request):
        file_id = request.GET.get("id", None)
        if (file_id == None):
            return JsonResponse({'code': []})
        initial = list(File.objects.filter(id=file_id))[0]
        sections = list(map(lambda x: x.content["code"], Section.objects.filter(
            file=initial).order_by('-start')))
        code = []
        for section in sections:
            code += section
        return JsonResponse({'code': code})



def get_tree(request):

    result = []
    if(request.user.is_authenticated):

        user = list(User.objects.filter(username = request.user.get_username()))[0]
        # print("user is: " + user.username)
        files = File.objects.filter(is_available=True)
        catalogs = Catalog.objects.filter(is_available=True)

        for catalog in catalogs:
            if catalog.parent_dir is None and catalog.owner == user:
                get_files(0, catalog, result)



    return result


def get_files(n, catalog, res):
    if (catalog.is_available == False):
        return False
    files = File.objects.filter(is_available=True)
    catalogs = Catalog.objects.filter(is_available=True)
    res.append([1, catalog, range(n)])
    for item in files:
        if (item.parent_dir == catalog):
            res.append([0, item, range(n+1)])
    for item in catalogs:
        if (item.parent_dir == catalog):
            res = get_files(n + 1, item, res)

    return res


# def save_file(request):
#     if request.method == "POST":
#         try:
#             data = request.POST

#             id = int(data.get("id"))
#             code = str(data.get("code"))
#             res = (code).split("\r\n")
#             write_file_to_db(id, res)
#         except Exception:
#             pass
#         return redirect('/polls/')

#     else:
#         return JsonResponse({'message': 'Invalid request method'})


# def write_file_to_db(fid, code):
#     f = list(File.objects.filter(id=fid))[0]
#     Section.objects.filter(file=f).delete()
#     # lines  = "\r\n".split(code)
#     # for i in range(len(code)):
#     #     res.append( list([i, code[i]]))
#     if (len(code) > 0):
#         create_section(0, len(code)-1, 'Function', code, f)
#     # check for comments
#     # code = check_for_comments(f, code)


# def create_section(a, b, skind, code, f, n="somename", desc=""):
#     k = None
#     try:
#         k = SectionKind.objects.filter(lambda x: x.name == skind).get()
#     except Exception:
#         k = None
#     out = []
#     for i in range(a, b + 1):
#         out.append(code[i])
#     Section.objects.create(name=str(n), description=str(
#         desc), add_date=timezone.now, start=a, end=b, kind=k, content={'code': out}, file=f)

@login_required
def get_compile_file(request):
    file_id = request.GET.get("id", None)
    if (file_id == None):
        return JsonResponse({'code': []})
    initial = list(File.objects.filter(id=file_id))[0]
    sections = list(map(lambda x: x.content["code"], Section.objects.filter(
    file=initial).order_by('-start')))
    code = []
    for section in sections:
        code += section
    assembly = compile(code, file_id)
    return JsonResponse({'code': assembly})



def compile(code, id):
    id = str(id)

    subprocess.run(["touch", "./c/" + id + ".c"])

    with open("./c/"+id+".c", "w") as f:
        for line in code:
            f.write(line + "\n")


    compilation1 = ["sdcc", "-S", "--std-" + standard, "-m" + processor]
    subprocess.run(["touch", "./c/"+id+"_err"])
    compilation2 = ["-o", "./c/" + id + ".asm", "./c/" + id+".c" ]
    for opt in optimisations:
        compilation1.append(opt)
    for opt in dependencies:
        compilation1.append(opt)
    warnings = subprocess.run(compilation1 + compilation2, capture_output=True, text=True)
    grep_result = subprocess.run(
        ["cat", "./c/" + id+".asm"], capture_output=True, text=True)
    
    res = grep_result.stdout if grep_result.stdout != "" else warnings.stderr 
    res = (res).split("\n")
    return res


def file_receiver(request):
    if request.method == "POST":
        try:
            data = request.POST
            folder = list(Catalog.objects.filter(id=data['folder_id']))[0]
            fname = data['name']
            fcode = str(data['code']).split("\r\n")
            u = list(User.objects.filter(username=request.user.get_username()))[0]
            f = File(name=fname, parent_dir=folder, owner=u)
            f.save()
            s = Section(name=fname+"_section", start=0, end=len(fcode),
                        file=f, content={'code': fcode})
            s.save()
        except Exception:
            pass
    return redirect('/polls/')


def delete_file(request):
    if request.method == "POST":
        try:
            id = request.POST.get('id', 0)
            File.objects.filter(id=int(id)).update(is_available=False)
        except Exception:
            pass

    return redirect('/polls/')


def delete_folder(request):
    if request.method == "POST":
        try:
            data = request.POST
            Catalog.objects.filter(id=int(data['id'])).update(is_available=False)
        except Exception:
            pass
    return redirect('/polls/')


def new_folder(request):
    if request.method == "POST":
        # pass
        # try:
            data = request.POST
            folder = data.get('catalog', None)
            n = data['name']
            if n == "":
                return redirect('/polls/')
            u = list(User.objects.filter(username=request.user.get_username()))[0]
            parent = get_element(list(Catalog.objects.filter(id=folder)), 0, None)
            cat = Catalog(name=n, owner=u, parent_dir=parent)
            cat.save()
        # except Exception:
        #     pass
    return redirect('/polls/')

def get_element(l, ind,  deff):
    if(len(l) <= ind ):
        return deff 
    return l[ind]


def compile_options(request):
    global standard
    global optimisations
    global processor
    global dependencies
    try:
        if request.method == "GET":
            data = request.GET
            standard = data['standard']
            optimisations = data.getlist('optimisations')
            processor = data['processor']
            dependencies = optimisations = data.getlist('dependencies')
    except Exception:
        pass
        # int(data)
    return redirect('/polls/')


def logout(request):
    request.session['uid'] = 0
    return redirect('/polls/')

