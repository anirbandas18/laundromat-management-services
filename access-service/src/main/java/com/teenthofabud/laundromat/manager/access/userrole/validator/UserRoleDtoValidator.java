package com.teenthofabud.laundromat.manager.access.userrole.validator;

import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.role.data.RoleException;
import com.teenthofabud.laundromat.manager.access.role.data.RoleVo;
import com.teenthofabud.laundromat.manager.access.role.service.RoleService;
import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleDto;
import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleMessageTemplate;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeException;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeVo;
import com.teenthofabud.laundromat.manager.access.usertype.service.UserTypeService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.Optional;

@Component
@Slf4j
public class UserRoleDtoValidator implements Validator {

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
        return clazz.isAssignableFrom(UserRoleDto.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        UserRoleDto dto = (UserRoleDto) target;

        Optional<String> optUserTypeId = dto.getUserTypeId();
        if(optUserTypeId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optUserTypeId.get()))) {
            boolean isValid = true;
            try {
                Long userTypeId = Long.parseLong(optUserTypeId.get());
                if(userTypeId <= 0L) {
                    isValid = false;
                    log.debug("UserRoleDto.userTypeId is invalid: userTypeId <= 0");
                } else {
                    UserTypeVo userType = userTypeService.retrieveDetailsById(userTypeId);
                    if(!userType.getActive()) {
                        isValid = false;
                        log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_DTO_USER_TYPE_ID_INACTIVE.getValue());
                    }
                }
            } catch (NumberFormatException | UserTypeException e) {
                isValid = false;
                log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_DTO_USER_TYPE_ID_INVALID.getValue());
                log.error(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_DTO_USER_TYPE_ID_INVALID.getValue(), e);
            }
            if(!isValid) {
                errors.rejectValue("userTypeId", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
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
                    log.debug("UserRoleDto.roleId is invalid: roleId <= 0");
                } else {
                    RoleVo role = roleService.retrieveDetailsById(roleId);
                    if(!role.getActive()) {
                        isValid = false;
                        log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_DTO_ROLE_ID_INACTIVE.getValue());
                    }
                }
            } catch (NumberFormatException | RoleException e) {
                isValid = false;
                log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_DTO_ROLE_ID_INVALID.getValue());
                log.error(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_DTO_ROLE_ID_INVALID.getValue(), e);
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
                log.debug("UserRoleDto.active is invalid");
                return;
            }
        }
    }
}
