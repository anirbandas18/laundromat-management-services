package com.teenthofabud.laundromat.manager.access.rolepermission.converter;

import com.teenthofabud.core.common.data.vo.TypeModelVo;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionEntity;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class RolePermissionEntity2VoConverter implements Converter<RolePermissionEntity, RolePermissionVo> {
    @Override
    public RolePermissionVo convert(RolePermissionEntity entity) {
        RolePermissionVo vo = new RolePermissionVo();
        vo.setPermissionId(entity.getPermission().getId());
        vo.setRoleId(entity.getRole().getId());
        vo.setId(entity.getId());
        vo.setActive(entity.getActive());
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }
}
