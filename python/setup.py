#!/usr/bin/env python

from setuptools import setup

setup(
	name='pyITEA',
	version='1.0.0',
	description='Python ITEA wrapper',
	author='Fabricio Olivetti de Franca',
	author_email='folivetti@ufabc.edu.br',
	url='https://github.com/folivetti/ITEA',
	packages=['pyITEA'],
	data_files=[('bin', ['itea'])],
    install_requires=['scikit-learn', 'numpy', 'pandas'],
	)
