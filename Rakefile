require 'rake/clean'

CLEAN.include '*.aux'
CLEAN.include '*.bbl'
CLEAN.include '*.blg'
CLEAN.include '*.log'
CLEAN.include '*.out'
CLOBBER.include 'prelim.pdf'

task :default => 'prelim.pdf'

rule '.pdf' => ['.tex'] do |t|
  jobname = File.basename(t.name, '.pdf')
  sh "pdflatex #{jobname}"
  if File.exists? File.basename(t.name, '.pdf') + '.bib'
    sh "bibtex #{jobname}"
    sh "pdflatex #{jobname}"
  end
  sh "pdflatex #{jobname}"
end
