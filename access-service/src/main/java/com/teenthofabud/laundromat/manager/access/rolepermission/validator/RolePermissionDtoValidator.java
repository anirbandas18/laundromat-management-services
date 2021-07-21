package com.teenthofabud.laundromat.manager.access.rolepermission.validator;

import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionException;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionVo;
import com.teenthofabud.laundromat.manager.access.permission.service.PermissionService;
import com.teenthofabud.laundromat.manager.access.role.data.RoleException;
import com.teenthofabud.laundromat.manager.access.role.data.RoleVo;
import com.teenthofabud.laundromat.manager.access.role.service.RoleService;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionDto;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionMessageTemplate;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.Optional;

@Component
@Slf4j
public class RolePermissionDtoValidator implements Validator {

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
        return clazz.isAssignableFrom(RolePermissionDto.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        RolePermissionDto dto = (RolePermissionDto) target;

        Optional<String> optPermissionId = dto.getPermissionId();
        if(optPermissionId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optPermissionId.get()))) {
            boolean isValid = true;
            try {
                Long permissionId = Long.parseLong(optPermissionId.get());
                if(permissionId <= 0L) {
                    isValid = false;
                    log.debug("RolePermissionDto.permissionId is invalid: permissionId <= 0");
                } else {
                    PermissionVo permission = permissionService.retrieveDetailsById(permissionId);
                    if(!permission.getActive()) {
                        isValid = false;
                        log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_DTO_PERMISSION_ID_INACTIVE.getValue());
                    }
                }
            } catch (NumberFormatException | PermissionException e) {
                isValid = false;
                log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_DTO_PERMISSION_ID_INVALID.getValue());
                log.error(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_DTO_PERMISSION_ID_INVALID.getValue(), e);
            }
            if(!isValid) {
                errors.rejectValue("permissionId", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
                return;
            }
        }


        Optional<String> optRoleId = dto.getRoleId();
        if(optRoleId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optRoleId.get()))) {
            boolean isValid = true;
            try {
                Long roleId = Long.parseLong(optRoleId.get());
                if(roleId <= 0L) {
                    isValid = false;
                    log.debug("RolePermissionDto.roleId is invalid: roleId <= 0");
                } else {
                    RoleVo role = roleService.retrieveDetailsById(roleId);
                    if(!role.getActive()) {
                        isValid = false;
                        log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_DTO_ROLE_ID_INACTIVE.getValue());
                    }
                }
            } catch (NumberFormatException | RoleException e) {
                isValid = false;
                log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_DTO_ROLE_ID_INVALID.getValue());
                log.error(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_DTO_ROLE_ID_INVALID.getValue(), e);
            }
            if(!isValid) {
                errors.rejectValue("roleId", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
                return;
            }
        }

        Optional<String> optActive = dto.getActive();
        if(optActive.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optActive.get()))) {
            Boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            Boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
                log.debug("RolePermissionDto.active is invalid");
                return;
            }
        }
    }
}
