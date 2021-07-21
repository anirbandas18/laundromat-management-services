package com.teenthofabud.laundromat.manager.access.userrole.validator;

import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.role.data.RoleException;
import com.teenthofabud.laundromat.manager.access.role.data.RoleVo;
import com.teenthofabud.laundromat.manager.access.role.service.RoleService;
import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleForm;
import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleMessageTemplate;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeException;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeVo;
import com.teenthofabud.laundromat.manager.access.usertype.service.UserTypeService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@Component
@Slf4j
public class UserRoleFormValidator implements Validator {

    private UserTypeService userTypeService;
    private RoleService roleService;

    @Autowired
    public void setUserTypeService(UserTypeService userTypeService) {
        this.userTypeService = userTypeService;
    }

    @Autowired
    public void setRoleService(RoleService roleService) {
        this.roleService = roleService;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(UserRoleForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        UserRoleForm form = (UserRoleForm) target;

        if(form.getUserTypeId() == null || form.getUserTypeId() <= 0L) {
            log.debug("PermissionForm.userTypeId is invalid");
            errors.rejectValue("userTypeId", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
            return;
        } else {
            try {
                UserTypeVo userType = userTypeService.retrieveDetailsById(form.getUserTypeId());
                if(!userType.getActive()) {
                    log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_DTO_USER_TYPE_ID_INACTIVE.getValue());
                    errors.rejectValue("userTypeId", AccessErrorCode.ACCESS_INACTIVE.name());
                    return;
                }
            } catch (UserTypeException e) {
                log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_DTO_USER_TYPE_ID_INVALID.getValue());
                log.error(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_DTO_USER_TYPE_ID_INVALID.getValue(), e);
                errors.rejectValue("userTypeId", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
                return;
            }
        }

        if(form.getRoleId() == null || form.getRoleId() <= 0L) {
            log.debug("PermissionForm.roleId is invalid");
            errors.rejectValue("roleId", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
            return;
        } else {
            try {
                RoleVo role = roleService.retrieveDetailsById(form.getRoleId());
                if(!role.getActive()) {
                    log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_DTO_ROLE_ID_INACTIVE.getValue());
                    errors.rejectValue("roleId", AccessErrorCode.ACCESS_INACTIVE.name());
                    return;
                }
            } catch (RoleException e) {
                log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_DTO_ROLE_ID_INVALID.getValue());
                log.error(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_DTO_ROLE_ID_INVALID.getValue(), e);
                errors.rejectValue("roleId", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
                return;
            }
        }
    }
}
