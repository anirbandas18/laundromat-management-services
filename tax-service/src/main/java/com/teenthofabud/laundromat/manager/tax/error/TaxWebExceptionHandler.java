package com.teenthofabud.laundromat.manager.tax.error;

import brave.Tracer;
import com.teenthofabud.core.common.handler.TOABBaseWebExceptionHandler;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.handler.TOABMessageSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice
public class TaxWebExceptionHandler implements TOABBaseWebExceptionHandler {

    @Autowired
    public void setMessageSource(TOABMessageSource messageSource) {
        this.messageSource = messageSource;
    }

    private TOABMessageSource messageSource;

    @Autowired
    public void setTracer(Tracer tracer) {
        this.tracer = tracer;
    }

    private Tracer tracer;

    @ExceptionHandler(TaxException.class)
    public ResponseEntity<ErrorVo> handleStudentException(TaxException e) {
        ResponseEntity<ErrorVo>  response = parseExceptionToResponse(e, messageSource, tracer);
        return response;
    }


}
