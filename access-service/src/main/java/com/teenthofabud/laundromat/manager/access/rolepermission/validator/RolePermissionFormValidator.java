package com.teenthofabud.laundromat.manager.access.rolepermission.validator;

import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionException;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionVo;
import com.teenthofabud.laundromat.manager.access.permission.service.PermissionService;
import com.teenthofabud.laundromat.manager.access.role.data.RoleException;
import com.teenthofabud.laundromat.manager.access.role.data.RoleVo;
import com.teenthofabud.laundromat.manager.access.role.service.RoleService;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionForm;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionMessageTemplate;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@Component
@Slf4j
public class RolePermissionFormValidator implements Validator {

    private PermissionService permissionService;
    private RoleService roleService;

    @Autowired
    public void setPermissionService(PermissionService permissionService) {
        this.permissionService = permissionService;
    }

    @Autowired
    public void setRoleService(RoleService roleService) {
        this.roleService = roleService;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(RolePermissionForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        RolePermissionForm form = (RolePermissionForm) target;

        if(form.getPermissionId() == null || form.getPermissionId() <= 0L) {
            log.debug("PermissionForm.permissionId is invalid");
            errors.rejectValue("permissionId", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
            return;
        } else {
            try {
                PermissionVo permission = permissionService.retrieveDetailsById(form.getPermissionId());
                if(!permission.getActive()) {
                    log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_DTO_PERMISSION_ID_INACTIVE.getValue());
                    errors.rejectValue("permissionId", AccessErrorCode.ACCESS_INACTIVE.name());
                    return;
                }
            } catch (PermissionException e) {
                log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_DTO_PERMISSION_ID_INVALID.getValue());
                log.error(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_DTO_PERMISSION_ID_INVALID.getValue(), e);
                errors.rejectValue("permissionId", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
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
                    log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_DTO_ROLE_ID_INACTIVE.getValue());
                    errors.rejectValue("roleId", AccessErrorCode.ACCESS_INACTIVE.name());
                    return;
                }
            } catch (RoleException e) {
                log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_DTO_ROLE_ID_INVALID.getValue());
                log.error(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_DTO_ROLE_ID_INVALID.getValue(), e);
                errors.rejectValue("roleId", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
                return;
            }
        }
    }
}
