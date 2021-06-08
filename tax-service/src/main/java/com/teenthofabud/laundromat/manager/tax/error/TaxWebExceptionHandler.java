package com.teenthofabud.laundromat.manager.tax.error;

import com.teenthofabud.core.common.handler.TOABBaseWebExceptionHandler;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice
public class TaxWebExceptionHandler implements TOABBaseWebExceptionHandler {

    @Autowired
    public void setMessageSource(MessageSource messageSource) {
        this.messageSource = messageSource;
    }

    private MessageSource messageSource;

    @ExceptionHandler(TaxException.class)
    public ResponseEntity<ErrorVo> handleStudentException(TaxException e) {
        ResponseEntity<ErrorVo>  response = parseExceptionToResponse(e, messageSource);
        return response;
    }


}
