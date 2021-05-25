package com.teenthofabud.core.common.handler;

import com.teenthofabud.core.common.model.error.TOABBaseException;
import com.teenthofabud.core.common.model.vo.ErrorVo;
import org.springframework.context.MessageSource;
import org.springframework.http.ResponseEntity;

import java.util.*;

public abstract class TOABBaseExceptionHandler {

    public ResponseEntity<ErrorVo> parseExceptionToResponse(TOABBaseException e, MessageSource messageSource) {
        ErrorVo vo = new ErrorVo();
        String msg = messageSource.getMessage(e.getError().getErrorCode(), null, Locale.US);
        if(e.getParameters() != null) {
            Deque<Object> parameters = new ArrayDeque<>(Arrays.asList(e.getParameters()));
            parameters.addFirst(e.getSubDomain().getName());
            msg = String.format(msg, parameters.toArray(new Object[parameters.size()]));
        }
        vo.setCode(e.getError().getErrorCode());
        vo.setMessage(msg);
        vo.setDomain(e.getError().getDomain());
        ResponseEntity<ErrorVo>  response = ResponseEntity.status(e.getError().getHttpStatusCode()).body(vo);
        return response;
    }


}
