package com.teenthofabud.laundromat.manager.access.userrole.converter;

import com.teenthofabud.core.common.handler.TOABBaseEntityAuditHandler;
import com.teenthofabud.laundromat.manager.access.role.data.RoleEntity;
import com.teenthofabud.laundromat.manager.access.role.repository.RoleRepository;
import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleEntity;
import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleForm;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeEntity;
import com.teenthofabud.laundromat.manager.access.usertype.repository.UserTypeRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@Slf4j
public class UserRoleForm2EntityConverter extends TOABBaseEntityAuditHandler implements Converter<UserRoleForm, UserRoleEntity> {

    private UserTypeRepository userTypeRepository;
    private RoleRepository roleRepository;

    @Autowired
    public void setUserTypeRepository(UserTypeRepository userTypeRepository) {
        this.userTypeRepository = userTypeRepository;
    }

    @Autowired
    public void setRoleRepository(RoleRepository roleRepository) {
        this.roleRepository = roleRepository;
    }

    @Override
    public UserRoleEntity convert(UserRoleForm form) {
        UserRoleEntity entity = new UserRoleEntity();
        Optional<UserTypeEntity> optUserType = userTypeRepository.findById(form.getUserTypeId());
        entity.setUserType(optUserType.get());
        Optional<RoleEntity> optRole = roleRepository.findById(form.getRoleId());
        entity.setRole(optRole.get());
        super.assignAuditValues(entity, Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }
}
