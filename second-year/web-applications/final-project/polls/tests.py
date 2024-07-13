import datetime
import random, string

from django.test import Client, TestCase
from django.utils import timezone
from django.urls import reverse
from django.contrib.auth.models import User
import json

from .models import *


# def create_question(question_text, days):
#     """
#     Create a question with the given `question_text` and published the
#     given number of `days` offset to now (negative for questions published
#     in the past, positive for questions that have yet to be published).
#     """
#     time = timezone.now() + datetime.timedelta(days=days)
#     return Question.objects.create(question_text=question_text, pub_date=time)

# def create_question_with_choice(question_text, days):
#     time = timezone.now() + datetime.timedelta(days=days)
#     question = Question.objects.create(
#         question_text=question_text, 
#         pub_date=time
#     )
#     Choice.objects.create(question=question, choice_text="option1")

#     return question
def randomword(length):
   letters = string.ascii_lowercase
   return ''.join(random.choice(letters) for i in range(length))
def create_catalog(n):
    user = create_user("test2")
    user.save()
    return Catalog(name=n, owner=user)

def create_user(n):
    return User(username=n, password='secret')

def create_file(n):
    user = create_user("test1")
    catalog = create_catalog("abcds")
    catalog.save()
    user.save()
    return File(name=n, owner=user, parent_dir=catalog)
def create_section(a, b):
    file = File.objects.first()
    return Section(file=file, start=a, end=b)


def login_self(self):
    self.credentials = {
            'username': 'testuser',
            'password': 'secret'}
    User.objects.create_user(**self.credentials)
    self.client.login(username=self.credentials['username'], password=self.credentials['password'])



def get_with_login(self):


        return self.client.get('/polls/', follow=True)



class CatalogModelTests(TestCase):
    
    def test_is_displayed(self):
        self.assertIs(create_catalog("test").is_available, True )
    def test_name_empty(self):
        with self.assertRaises(Exception):
            file = create_catalog("")
            file.save()
    def test_same_name(self):
        with self.assertRaises(Exception):
            file1 = create_catalog("same")
            file2 = create_catalog("same")
            file1.save()
            file2.save()

    def test_user_null(self):
        with self.assertRaises(Exception):
            parent_dir = Catalog.objects.first()
            File(name="nouser",owner=None, parent_dir=parent_dir).save()
    def test_parent_null(self):
        with self.assertRaises(Exception):
            owner = User.objects.first()
            File(name="nofile", owner=owner, parent_dir=None).save()


class FileModelTests(TestCase):
    
    def test_is_displayed(self):
        file = create_file("test")
        file.save()
        self.assertIs(file.is_available, True )
    def test_name_empty(self):
        with self.assertRaises(Exception):
            file = create_file("")
            file.save()
    def test_same_name(self):
        with self.assertRaises(Exception):
            catalog1 = create_file("same")
            cataog2 = create_file("same")
            catalog1.save()
            cataog2.save()

    def test_user_null(self):
        with self.assertRaises(Exception):
            Catalog(name="nouser",owner=None).save()


class SectionModelTests(TestCase):
    def test_negative_start(self):
        with self.assertRaises(Exception):
            section = create_section(-1, 1)
            section.save()
    def test_start_l_end(self):
        with self.assertRaises(Exception):
            section = create_section(2, 1)
            section.save()




class IndexViewTests(TestCase):

    def test_not_logged_in(self):

         response = self.client.get(reverse('polls:index'))
         self.assertContains(response, "Please, log in")
    def fill_files(self):
        login_self(self)
        for i in range(9):
             n = randomword(11)
             self.client.post('/polls/new_folder/', {'name': n})
             folder_id = Catalog.objects.filter(name = n).first().id
             for j in range(9):
                  self.client.post('/polls/file_receiver', {'name': randomword(11), 'code': randomword(11), 'folder_id': folder_id})



    def test_with_login(self):
        
        self.fill_files()
        response = get_with_login(self)
        self.assertNotContains(response, "No polls are available.")




class DeleteFileFormTest(TestCase):
    def test_nonexisting_file(self):
        login_self(self)
        response = self.client.post('/polls/delete_file', {'id':-11})
        self.assertEqual(response.status_code, 302)
    def test_empty_string(self):
        login_self(self)
        response = self.client.post('/polls/delete_file', {})
        self.assertEqual(response.status_code, 302)



class SaveGetFileTest(TestCase):
    def test_create_folder(self):
        login_self(self)
        n = 'testfolder'
        response = self.client.post('/polls/new_folder/', {'name': n})

        catalog = Catalog.objects.filter(name = n).first()
        # print(catalog)
        self.assertNotEqual(catalog, None)
        return catalog

    def test_create_file(self): 
        # login_self(self)
        catalog = self.test_create_folder()
        self.assertEqual(catalog.name, 'testfolder')
        n = 'testfile'
        code = 'somecode\r\n'
        # print("FLSDFDSFLKSMFKL "  + str(catalog.id))
        response = self.client.post('/polls/file_receiver', {'name': n, 'code': code, 'folder_id': catalog.id})
        file = File.objects.filter(name = n).first()
        self.assertNotEqual(file, None)
        return file, code 
    def test_text_file(self):
        file, code = self.test_create_file()
        response = self.client.get('/polls/get_file_text', {"id": file.id})
        self.assertEqual(str('\r\n'.join(json.loads(response.content)['code'])) ,str(code))
        return file
    def test_compile_options(self):
        file, code = self.test_create_file()
        response = self.client.get('/polls/compile_options', {'standart': 'c11'})
        self.assertEqual(response.status_code, 302)

        
    def test_compile_file(self):
        file = self.test_text_file()
        response = self.client.get('/polls/get_compile_file', {"id": file.id})

        self.assertIsNotNone(str(response.content) )
    

#
class DeleteFolderFormTest(TestCase):
    def test_nonexisting_folder(self):
        login_self(self)
        response = self.client.post('/polls/delete_folder', {'id':-11})
        self.assertEqual(response.status_code, 302)
    def test_empty_string(self):
        login_self(self)
        response = self.client.post('/polls/delete_folder', {})
        self.assertEqual(response.status_code, 302)

class FileReceiverFormTest(TestCase):
    def test_empty_name(self):
        login_self(self)
        response = self.client.post('/polls/file_receiver', {'name':'','code':'1323', 'folder_id':1})
        self.assertEqual(response.status_code, 302)
        self.assertEqual(len(File.objects.filter(parent_dir_id=1).filter(name="")), 0)

    def test_bad_parent_id(self):
        login_self(self)
        response = self.client.post('/polls/file_receiver', {'name':'some','code':'1323', 'folder_id':123123})
        self.assertEqual(response.status_code, 302)
        self.assertEqual(len(File.objects.filter(parent_dir_id=123123).filter(name="some")), 0)
class NewFolderFormTest(TestCase):
    def test_empty_name(self):
        login_self(self)
        response = self.client.post('/polls/new_folder/', {'name':''})
        self.assertEqual(response.status_code, 302)
        self.assertEqual(len(Catalog.objects.filter(parent_dir=None).filter(name="")), 0)
    def test_bad_parent_id(self):
        login_self(self)
        response = self.client.post('/polls/new_folder/', {'name':'some', 'catalog':123123})
        self.assertEqual(response.status_code, 302)
        self.assertEqual(len(File.objects.filter(parent_dir_id=123123).filter(name="some")), 0)



