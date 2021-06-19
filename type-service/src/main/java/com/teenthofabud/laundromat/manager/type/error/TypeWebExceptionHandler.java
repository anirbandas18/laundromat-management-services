package com.teenthofabud.laundromat.manager.type.error;

import brave.Tracer;
import com.teenthofabud.core.common.handler.TOABBaseWebExceptionHandler;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.handler.TOABMessageSource;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVException;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice
public class TypeWebExceptionHandler implements TOABBaseWebExceptionHandler {

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

    @ExceptionHandler(TypeLOVException.class)
    public ResponseEntity<ErrorVo> handleTypeLOVException(TypeLOVException e) {
        ResponseEntity<ErrorVo>  response = parseExceptionToResponse(e, messageSource, tracer);
        return response;
    }

    @ExceptionHandler(TypeModelException.class)
    public ResponseEntity<ErrorVo> handleTypeModelException(TypeModelException e) {
        ResponseEntity<ErrorVo>  response = parseExceptionToResponse(e, messageSource, tracer);
        return response;
    }

}
