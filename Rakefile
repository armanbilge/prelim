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
  sh "bibtex #{jobname}"
  2.times do
    sh "pdflatex #{jobname}"
  end
end
