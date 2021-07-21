package com.teenthofabud.laundromat.manager.access.rolepermission.validator;

import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

@Component
@Slf4j
public class RolePermissionFormRelaxedValidator implements RelaxedValidator<RolePermissionForm>  {
    @Override
    public Boolean validateLoosely(RolePermissionForm form, Errors errors) {
        log.debug("RolePermissionForm.permissionId is valid");
        if(form.getPermissionId() != null && form.getPermissionId() <= 0L) {
            errors.rejectValue("permissionId", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
            log.debug("RolePermissionForm.permissionId is empty");
            return false;
        }
        log.debug("RolePermissionForm.permissionId is valid");

        log.debug("RolePermissionForm.roleId is valid");
        if(form.getRoleId() != null && form.getRoleId() <= 0L) {
            errors.rejectValue("roleId", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
            log.debug("RolePermissionForm.roleId is empty");
            return false;
        }
        log.debug("RolePermissionForm.roleId is valid");
        return true;
    }
}
