# -*- coding: utf-8 -*-
__revision__ = "$Id: $"

import sys
import os

from setuptools import setup, find_packages
from openalea.deploy.metainfo import read_metainfo

# Packages list, namespace and root directory of packages

pkg_root_dir = 'src'
namespace = 'openalea'
pkgs = [ pkg for pkg in find_packages(pkg_root_dir)]
top_pkgs = [pkg for pkg in pkgs if  len(pkg.split('.')) < 2]
packages = pkgs
package_dir = dict( [('',pkg_root_dir)] + [(namespace + "." + pkg, pkg_root_dir + "/" + pkg) for pkg in top_pkgs] )

setup(
    name='openalea.vmango',
    version='1.0.0',
    description='V-Mango package for openalea.',
    long_description='The openalea.vmango contains a simulation and analysis tool for mango tree architecture.' ,
    author='Frederic Boudon',
    author_email='frederic.boudon@cirad.fr',
    url='http://openalea.gforge.inria.fr',
    license='Cecill-C',
    keywords = '',	

    # package installation
    packages= 'vmango',	
    package_dir= package_dir,

    # Namespace packages creation by deploy
    #namespace_packages = [namespace],
    #create_namespaces = False,
    zip_safe= False,

    # Eventually include data in your package
    # (flowing is to include all versioned files other than .py)
    include_package_data = True,
    # (you can provide an exclusion dictionary named exclude_package_data to remove parasites).
    # alternatively to global inclusion, list the file to include   
    #package_data = {'' : ['*.pyd', '*.so'],},

    # postinstall_scripts = ['',],

    # Declare scripts and entry_points (extensions) of your package 
    entry_points = { 
        #'console_scripts': [
        #       'fake_script = openalea.fakepackage.amodule:console_script', ],
        # 'gui_scripts': [
        #      'fake_gui = openalea.fakepackage.amodule:gui_script',],
        #
        },
    )


