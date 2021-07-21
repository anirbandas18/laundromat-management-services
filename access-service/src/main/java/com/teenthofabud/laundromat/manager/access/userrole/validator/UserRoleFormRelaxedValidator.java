package com.teenthofabud.laundromat.manager.access.userrole.validator;

import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

@Component
@Slf4j
public class UserRoleFormRelaxedValidator implements RelaxedValidator<UserRoleForm>  {
    @Override
    public Boolean validateLoosely(UserRoleForm form, Errors errors) {
        log.debug("UserRoleForm.userTypeId is valid");
        if(form.getUserTypeId() != null && form.getUserTypeId() <= 0L) {
            errors.rejectValue("userTypeId", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
            log.debug("UserRoleForm.userTypeId is empty");
            return false;
        }
        log.debug("UserRoleForm.userTypeId is valid");

        log.debug("UserRoleForm.roleId is valid");
        if(form.getRoleId() != null && form.getRoleId() <= 0L) {
            errors.rejectValue("roleId", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
            log.debug("UserRoleForm.roleId is empty");
            return false;
        }
        log.debug("UserRoleForm.roleId is valid");
        return true;
    }
}
