# HDF5-Fortran

  **Se possível use o HDF5 do python, é muito mais fácil.**

  A ideia é utilizar o fortran o minímo possível, só para salvar e compactar os dados, qualquer
análise posterior vai ser feita usando o python. Desta forma, deixei alguns exemplos de como
salvar dados usando HDF5-fortran, o que você vai encontrar nos arquivos:

- Criar e salvar um dataset.
- Criar grupos.
- Escrever comentários nos dados.
- Salvar dados parciais, i.e., ir acrescentando os dados a médida que o programa roda.
(Usei chunck para isso, mas pode ser feito extendendo os dataset)
- Compactar o arquivo.

  > **Para compilar:**
  >
  >  hf5c nome_do_programa.f90.

  > Utilidade:
  > O comando hf5c -show mostra as flags que estão sendo usadas. Isso é interessante para
  > ver a localização das bibliotecas e como elas são linkadas. 


**Referências**

[Github](https://github.com/mokus0/hdf5/tree/master/fortran/examples)

[HDFGroup](https://support.hdfgroup.org/HDF5/examples/api-fortran.html)
