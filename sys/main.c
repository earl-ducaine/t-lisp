
#define STORED_TAG 2
#define t_procedure(name) &name[STORED_TAG]

extern char sample[];

main(argc,argv) char **argv; {
	printf("sample (%d) = %d\n",
	       3,
	       call_t_procedure(t_procedure(sample), 3));
}
