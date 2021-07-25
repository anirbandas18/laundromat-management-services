package com.teenthofabud.laundromat.manager.access.error;

import brave.Tracer;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.handler.TOABBaseWebExceptionHandler;
import com.teenthofabud.core.common.handler.TOABMessageSource;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationException;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionException;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceException;
import com.teenthofabud.laundromat.manager.access.role.data.RoleException;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionException;
import com.teenthofabud.laundromat.manager.access.securityquestion.data.SecurityQuestionException;
import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleException;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice
public class AccessWebExceptionHandler implements TOABBaseWebExceptionHandler {

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

    @ExceptionHandler(value = { ResourceException.class, OperationException.class, PermissionException.class,
            SecurityQuestionException.class, UserTypeException.class, RoleException.class, UserRoleException.class,
            RolePermissionException.class })
    public ResponseEntity<ErrorVo> handleAccessSubDomainExceptions(TOABBaseException e) {
        ResponseEntity<ErrorVo>  response = parseExceptionToResponse(e, messageSource, tracer);
        return response;
    }

}
