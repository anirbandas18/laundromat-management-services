package com.teenthofabud.laundromat.manager.access.usertype.converter;

import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeEntity;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class UserTypeEntity2VoConverter implements Converter<UserTypeEntity, UserTypeVo> {
    @Override
    public UserTypeVo convert(UserTypeEntity entity) {
        UserTypeVo vo = new UserTypeVo();
        vo.setName(entity.getName());
        vo.setDescription(entity.getDescription());
        vo.setId(entity.getId());
        vo.setActive(entity.getActive());
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }
}
