EDOC_PATH=../../../tools/utilities

#single place to include docs from.
docs:
	@mkdir -p ../doc
	@echo -n $${MY_BLUE:-$(BLUE)}; \
	$(EDOC_PATH)/edoc $(APP_NAME); \
	if [ $$? -eq 0 ]; then \
		echo $${MY_LRED:-$(LRED)}"$$d Doc Failed"; \
	fi; \
	echo -n $(OFF)$(NO_COLOR)	

