package com.teenthofabud.laundromat.manager.access.permission.validator;

import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionForm;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.math.NumberUtils;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

@Component
@Slf4j
public class PermissionFormRelaxedValidator implements RelaxedValidator<PermissionForm>  {
    @Override
    public Boolean validateLoosely(PermissionForm form, Errors errors) {
        log.debug("PermissionForm.resourceId is valid");
        if(form.getResourceId() != null && form.getResourceId() <= 0L) {
            errors.rejectValue("resourceId", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
            log.debug("PermissionForm.resourceId is empty");
            return false;
        }
        log.debug("PermissionForm.resourceId is valid");

        log.debug("PermissionForm.operationId is valid");
        if(form.getOperationId() != null && form.getOperationId() <= 0L) {
            errors.rejectValue("operationId", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
            log.debug("PermissionForm.operationId is empty");
            return false;
        }
        log.debug("PermissionForm.operationId is valid");
        return true;
    }
}
