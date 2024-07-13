from django.contrib import admin
from .models import Catalog, File, SectionKind, SectionStatus, StatusData, Section

# admin.site.register(Question)
# admin.site.register(Choice)
admin.site.register(Catalog)
admin.site.register(File)
admin.site.register(SectionKind)
admin.site.register(SectionStatus)
admin.site.register(StatusData)
admin.site.register(Section)

# Register your models here.
