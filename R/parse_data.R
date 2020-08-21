#this is an insane list of the columns we want to retain
keep = c("First.Name", "Last.Name", "Aging", "Bacteriology", "Behavior", 
  "Biochemistry", "Bioengineering", "Bioinformatics", "Biomechanics", 
  "Biophysics", "Biotechnology", "Cancer", "Cell.biology", "Cell.division..cell.cycle", 
  "Cell.organization.and.polarity", "Cell.signaling...signal.transduction", 
  "Chromosome.biology", "Clinical.and.translational.research", 
  "Cognition", "Computational.biology", "Cytoskeleton", "Data.and.information.science", 
  "Developmental.biology", "Developmental.disorders", "Drug.development", 
  "Electrophysiology", "Endocrinology", "Epigenetics", "Evolutionary.biology", 
  "Gene.regulation", "Genetics", "Genome.editing...genetic.engineering", 
  "Genomics", "Health.and.disease", "Hematopoiesis", "Human.genetic.disorders", 
  "Imaging...detection.systems", "Immune.systems", "Lipids.and.membranes", 
  "Mathematical.biology", "Metabolism", "Metabolomics", "Molecular.biology", 
  "Mycology", "Nanobiology", "Neurobiology", "Neurological...cognitive...behavioral.disorders", 
  "Non.coding.RNAs", "Nuclear.organization..3D.4D.nucleome", "Nucleic.acids..DNA..RNA", 
  "Organelles..trafficking", "Organogenesis", "Parasitology", "Pathogen.host.interactions", 
  "Pathology", "Patterning.and.cell.fate.determination", "Pharmacology", 
  "Physiology", "Population.genetics", "Proteins..proteomics", 
  "Quantitative...statistical.biology", "Regenerative.biology", 
  "Regenerative.medicine", "Reproductive.biology", "Sensory.biology", 
  "Stem.cells", "Structural.biology", "Synthetic.biology", "Systems.biology...integrative.biology...complex.biosystems", 
  "Virology", "Bacteria...Prokaryotes", "C..elegans", "Cell.culture", 
  "Cell.free.systems", "Chicken", "Drosophila", "Fish", "Frog", 
  "Human", "In.silico.models", "Mouse..Rat", "Plants", "Stem.cells.and.organoids", 
  "Viruses", "Yeast..other.fungi", "Other...Nontraditional.model.systems" )

parse_data = function( faculty_file, 
                     student_file,
                     keep_cols = keep ){
  
  faculty = read.csv( faculty_file, stringsAsFactors = FALSE )
  faculty = faculty[ keep_cols ]
  
  #gets all names properly capitalized
  faculty$First.Name = proper_names( faculty$First.Name )
  faculty$Last.Name = proper_names( faculty$Last.Name )
  
  #combines names into a new column: Last name, First name
  faculty$Name = paste( faculty$Last.Name, faculty$First.Name, sep = ', ' )
  
  #drops the individual name columns
  drop = c( 'First.Name', 'Last.Name' )
  faculty = faculty[ , !( names( faculty ) %in% drop ) ]
  
  #moves Name to front of df
  faculty = faculty[ , c( length( names( faculty ) ), 1:length( names( faculty ) ) -1 ) ]
  
  
  
  
  
  student = read.csv( student_file, stringsAsFactors = FALSE  )
  student = student[ keep_cols ]
  
  #gets all names properly capitalized
  student$First.Name = proper_names( student$First.Name )
  student$Last.Name = proper_names( student$Last.Name )
  
  #combines names into a new column: Last name, First name
  student$Name = paste( student$Last.Name, student$First.Name, sep = ', ' )
  
  #drops the individual name columns
  student = student[ , !( names( student ) %in% drop ) ]
  
  #moves Name to front of df
  student = student[ , c( length( names( student ) ), 1:length( names( student ) ) -1 ) ]
  
  if ( !( all( names( faculty ) == names( student ) ) ) ){
    print( 'Column names do not match between students and faculty - You are in danger!' )
  }
  
  return( list( 'faculty' = faculty, 'student' = student ) )
  
}

proper_names = function( name ){
  
  substr( name, 1, 1 ) = toupper( substr( name, 1, 1 ) )
  substr( name, 2, length( name ) + 1 ) = tolower( substr( name, 2, length( name ) + 1 ) )
  
  return ( name )
}