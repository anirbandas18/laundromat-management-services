package com.teenthofabud.core.common.handler;

import brave.Tracer;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import org.springframework.context.MessageSource;
import org.springframework.http.ResponseEntity;

import java.util.*;

public interface TOABBaseWebExceptionHandler {

    @Deprecated
    default ResponseEntity<ErrorVo> parseExceptionToResponse(TOABBaseException e, MessageSource messageSource) {
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
        return ResponseEntity.status(e.getError().getHttpStatusCode()).body(vo);
    }

    default ResponseEntity<ErrorVo> parseExceptionToResponse(TOABBaseException e, TOABMessageSource messageSource, Tracer tracer) {
        ErrorVo vo = new ErrorVo();
        String msg = messageSource.getMessage(e.getError().getErrorCode(), null, Locale.getDefault());
        if(e.getParameters() != null) {
            Deque<Object> parameters = new ArrayDeque<>(Arrays.asList(e.getParameters()));
            parameters.addFirst(e.getSubDomain().getName());
            msg = String.format(msg, parameters.toArray(new Object[parameters.size()]));
        }
        String domain = String.join(" - ", Arrays.asList(e.getError().getDomain(), e.getSubDomain().getName()));
        vo.setCode(e.getError().getErrorCode());
        vo.setMessage(msg);
        vo.setDomain(domain);
        vo.setTrace(tracer.currentSpan().context().traceIdString());
        return ResponseEntity.status(e.getError().getHttpStatusCode()).body(vo);
    }


}
