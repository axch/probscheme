# -*- ruby -*-

### ----------------------------------------------------------------------
### Copyright 2007 Alexey Radul, Taylor Campbell, and Yu-hsin Chen.
### ----------------------------------------------------------------------
### This file is part of Probabilistic Scheme.
### 
### Probabilistic Scheme is free software; you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published by
### the Free Software Foundation, either version 3 of the License, or
### (at your option) any later version.
### 
### Probabilistic Scheme is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.
### 
### You should have received a copy of the GNU General Public License
### along with Probabilistic Scheme.  If not, see <http://www.gnu.org/licenses/>.
### ----------------------------------------------------------------------

require 'rake'

task :default => :test

task :test do 
  sh %Q{mit-scheme --heap 6500 --batch-mode --load load-probscheme --load all-tests.scm --eval "(run-tests-and-exit)"}
end

LATEX_DIRS=%w{class-proposal class-report ita_talk oopsla_talk dls2007 mltea_talk}.map { |name| "publications/#{name}" }

task :clean_latex do
  LATEX_DIRS.each do |dir|
    sh "cd #{File.dirname(__FILE__) + '/' + dir}; rm -f *.aux *.log *.nav *.out *.snm *.toc *.bbl *.blg"
  end
end

task :clean_compiled do
  %w(.bin .bci .com .ext ~).each do |extension|
    sh "find . -name \"*#{extension}\" -delete"
  end
end

task :clean => [:clean_latex, :clean_compiled] do
  sh "cd #{File.dirname(__FILE__)}; find . -name '*~' | xargs rm -f; find . -name 'actions.log' | xargs rm -f"
end

task :release => :clean do
  sh "cd #{File.dirname(__FILE__)}; " + %Q{tar --create --verbose --file ../probscheme.tar --directory .. --exclude="*.git*" --exclude="*amb-examples*" --exclude=.commitmail --exclude="*doc*" --exclude="todo*.txt" probscheme/}
end
