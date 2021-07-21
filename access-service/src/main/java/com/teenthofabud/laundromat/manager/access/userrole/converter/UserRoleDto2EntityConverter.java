package com.teenthofabud.laundromat.manager.access.userrole.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.role.data.RoleEntity;
import com.teenthofabud.laundromat.manager.access.role.data.RoleException;
import com.teenthofabud.laundromat.manager.access.role.data.RoleVo;
import com.teenthofabud.laundromat.manager.access.role.repository.RoleRepository;
import com.teenthofabud.laundromat.manager.access.role.service.RoleService;
import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleDto;
import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleEntity;
import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleException;
import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleMessageTemplate;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeEntity;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeException;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeVo;
import com.teenthofabud.laundromat.manager.access.usertype.repository.UserTypeRepository;
import com.teenthofabud.laundromat.manager.access.usertype.service.UserTypeService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Arrays;
import java.util.Collections;
import java.util.Optional;

@Component
@Slf4j
public class UserRoleDto2EntityConverter implements ComparativePatchConverter<UserRoleDto, UserRoleEntity> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 3;

    private UserTypeService userTypeService;
    private RoleService roleService;
    private UserTypeRepository userTypeRepository;
    private RoleRepository roleRepository;

    @Autowired
    public void setUserTypeService(UserTypeService userTypeService) {
        this.userTypeService = userTypeService;
    }

    @Autowired
    public void setRoleService(RoleService roleService) {
        this.roleService = roleService;
    }

    @Autowired
    public void setUserTypeRepository(UserTypeRepository userTypeRepository) {
        this.userTypeRepository = userTypeRepository;
    }

    @Autowired
    public void setRoleRepository(RoleRepository roleRepository) {
        this.roleRepository = roleRepository;
    }

    @Override
    public void compareAndMap(UserRoleDto dto, UserRoleEntity actualEntity) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;

        Optional<String> optUserTypeId = dto.getUserTypeId();
        if(optUserTypeId.isPresent()) {
            Long userTypeId = Long.parseLong(optUserTypeId.get());
            try {
                UserTypeVo userType = userTypeService.retrieveDetailsById(userTypeId);
                if(!userType.getActive()) {
                    log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_DTO_USER_TYPE_ID_INACTIVE.getValue());
                    throw new UserRoleException(AccessErrorCode.ACCESS_INACTIVE, new Object[] { "userTypeId" });
                }
            } catch (UserTypeException e) {
                log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_DTO_USER_TYPE_ID_INVALID.getValue());
                log.error(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_DTO_USER_TYPE_ID_INVALID.getValue(), e);
                throw new UserRoleException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "userTypeId" });
            }
            Optional<UserTypeEntity> optUserType = userTypeRepository.findById(userTypeId);
            actualEntity.setUserType(optUserType.get());
            changeSW[i++] = true;
            log.debug("UserRoleDto.userTypeId is valid");
        }

        Optional<String> optRoleId = dto.getRoleId();
        if(optRoleId.isPresent()) {
            Long roleId = Long.parseLong(optRoleId.get());
            try {
                RoleVo role = roleService.retrieveDetailsById(roleId);
                if(!role.getActive()) {
                    log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_DTO_ROLE_ID_INACTIVE.getValue());
                    throw new UserRoleException(AccessErrorCode.ACCESS_INACTIVE, new Object[] { "roleId" });
                }
            } catch (RoleException e) {
                log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_DTO_ROLE_ID_INVALID.getValue());
                log.error(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_DTO_ROLE_ID_INVALID.getValue(), e);
                throw new UserRoleException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "roleId" });
            }
            Optional<RoleEntity> optRole = roleRepository.findById(roleId);
            actualEntity.setRole(optRole.get());
            changeSW[i++] = true;
            log.debug("UserRoleDto.roleId is valid");
        }

        Optional<String> optActive = dto.getActive();
        if(optActive.isPresent()) {
            actualEntity.setActive(Boolean.valueOf(optActive.get()));
            changeSW[i++] = true;
            log.debug("UserRoleDto.active is valid");
        }
        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) >= 1) {
            log.debug("All provided UserRoleDto attributes are valid");
            actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided UserRoleDto attributes are valid");
    }
}
