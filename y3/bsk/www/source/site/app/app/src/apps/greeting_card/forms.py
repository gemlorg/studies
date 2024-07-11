from django import forms


class GreetingCardForm(forms.Form):
    recipient = forms.TextInput()
    content = forms.TextInput()
