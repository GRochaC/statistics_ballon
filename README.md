# Statistics Balloon
Script de web scraping em R que coleta informações dos integrantes do grupo de estudo UnBalloon da Universidade de Brasília no site Codeforces.

## Descrição
O Script é capaz de extrair as handles dos afiliados à UnB no Codeforces, e com isso é capaz de coletar a divisão, rating, divisão máxima, rating máximo e total de problemas resolvidos pela pessoa.

Com essas informações, o script gera um arquivo .csv com tais informações e, além disso, cria alguns gráficos que permitem a visualização do desempenho geral do grupo de estudos no website. Esses arquivos são salvos no diretório `dados` na pasta nomeada com o dia que o script foi executado.

## Dependências
* R

## Executando o projeto
* Entre na pasta
```
cd statistics_ballon
```
* Execute o comando
```
Rscript scrapper.R
```