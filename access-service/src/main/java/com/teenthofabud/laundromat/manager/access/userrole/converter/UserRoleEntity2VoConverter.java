package com.teenthofabud.laundromat.manager.access.userrole.converter;

import com.teenthofabud.core.common.data.vo.TypeModelVo;
import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleEntity;
import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class UserRoleEntity2VoConverter implements Converter<UserRoleEntity, UserRoleVo> {
    @Override
    public UserRoleVo convert(UserRoleEntity entity) {
        UserRoleVo vo = new UserRoleVo();
        TypeModelVo userType = new TypeModelVo();
        userType.setName(entity.getUserType().getName());
        userType.setId(entity.getUserType().getId());
        vo.setUserType(userType);
        TypeModelVo role = new TypeModelVo();
        role.setName(entity.getRole().getName());
        role.setId(entity.getRole().getId());
        vo.setRole(role);
        vo.setId(entity.getId());
        vo.setActive(entity.getActive());
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }
}
