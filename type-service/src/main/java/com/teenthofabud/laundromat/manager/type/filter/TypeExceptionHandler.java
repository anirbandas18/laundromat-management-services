package com.teenthofabud.laundromat.manager.type.filter;

import com.teenthofabud.core.common.handler.TOABBaseExceptionHandler;
import com.teenthofabud.core.common.model.vo.ErrorVo;
import com.teenthofabud.laundromat.manager.type.model.error.TypeException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice
public class TypeExceptionHandler extends TOABBaseExceptionHandler {

    @Autowired
    public void setMessageSource(MessageSource messageSource) {
        this.messageSource = messageSource;
    }

    private MessageSource messageSource;

    @ExceptionHandler(TypeException.class)
    public ResponseEntity<ErrorVo> handleStudentException(TypeException e) {
        ResponseEntity<ErrorVo>  response = super.parseExceptionToResponse(e, messageSource);
        return response;
    }


}
