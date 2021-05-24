package com.teenthofabud.core.common.handler;

import com.teenthofabud.core.common.model.error.TOABBaseException;
import com.teenthofabud.core.common.model.vo.ErrorVo;
import org.springframework.context.MessageSource;
import org.springframework.http.ResponseEntity;

import java.util.Locale;

public abstract class TOABBaseExceptionHandler {

    public ResponseEntity<ErrorVo> parseExceptionToResponse(TOABBaseException e, MessageSource messageSource) {
        ErrorVo vo = new ErrorVo();
        String msg = messageSource.getMessage(e.getError().getErrorCode(), null, Locale.US);
        msg = e.getParameters() != null ? String.format(msg, e.getParameters()) : msg;
        vo.setCode(e.getError().getErrorCode());
        vo.setMessage(msg);
        vo.setDomain(e.getError().getDomain());
        ResponseEntity<ErrorVo>  response = ResponseEntity.status(e.getError().getHttpStatusCode()).body(vo);
        return response;
    }


}
