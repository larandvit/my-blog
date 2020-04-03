#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

AUTHOR = 'Vitaly Saversky'
SITENAME = 'tech jogging'
SITEURL = ''

PATH = 'content'

TIMEZONE = 'America/Toronto'

DEFAULT_LANG = 'en'

# Feed generation is usually not desired when developing
FEED_ALL_ATOM = None
CATEGORY_FEED_ATOM = None
TRANSLATION_FEED_ATOM = None
AUTHOR_FEED_ATOM = None
AUTHOR_FEED_RSS = None
DISPLAY_CATEGORIES_ON_MENU = None

SHOW_ARTICLE_CATEGORY = True

DISPLAY_BREADCRUMBS = False
DISPLAY_CATEGORY_IN_BREADCRUMBS = False

DISPLAY_ARTICLE_INFO_ON_INDEX = True

DISPLAY_TAGS_ON_SIDEBAR = False
DISPLAY_TAGS_INLINE = False

DISPLAY_CATEGORIES_ON_SIDEBAR = True

DISPLAY_RECENT_POSTS_ON_SIDEBAR = True

DEFAULT_PAGINATION = False

PLUGINS = [ 'i18n_subsites', 'tipue_search', 'more_categories', 'minify' ]
PLUGIN_PATHS = [ 'pelican-plugins/', ]

THEME = "pelican-themes/pelican-bootstrap3"
JINJA_ENVIRONMENT = {'extensions': ['jinja2.ext.i18n']}

BOOTSTRAP_THEME = 'flatly'
PYGMENTS_STYLE = 'monokai'

DIRECT_TEMPLATES = ['index', 'categories', 'search']

GOOGLE_ANALYTICS = 'UA-156524189-1'

ARTICLE_PATHS = ['articles']
ARTICLE_EXCLUDES = ['extra']

STATIC_PATHS = ['extra']
EXTRA_PATH_METADATA = {
    'extra/favicon.ico': {'path': 'favicon.ico'},
    'extra/BingSiteAuth.xml': {'path': 'BingSiteAuth.xml'},
    'extra/robots.txt': {'path': 'robots.txt'},
    'extra/sitemap.xml': {'path': 'sitemap.xml'},
    'extra/yandex_96a264214b85ba90.html': {'path': 'yandex_96a264214b85ba90.html'},
    'extra/missing': {'path': 'missing'}
}

SITELOGO = 'extra/site-logo.png'

DISQUS_SITENAME = 'techjogging'

MINIFY = {
  'remove_comments': True,
  'remove_all_empty_space': True,
  'remove_optional_attribute_quotes': False
}

DELETE_OUTPUT_DIRECTORY = True

# Uncomment following line if you want document-relative URLs when developing
#RELATIVE_URLS = True
