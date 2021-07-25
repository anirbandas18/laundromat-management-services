package com.teenthofabud.laundromat.manager.access.securityquestion.converter;

import com.teenthofabud.laundromat.manager.access.securityquestion.data.SecurityQuestionEntity;
import com.teenthofabud.laundromat.manager.access.securityquestion.data.SecurityQuestionVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class SecurityQuestionEntity2VoConverter implements Converter<SecurityQuestionEntity, SecurityQuestionVo> {
    @Override
    public SecurityQuestionVo convert(SecurityQuestionEntity entity) {
        SecurityQuestionVo vo = new SecurityQuestionVo();
        vo.setName(entity.getName());
        vo.setId(entity.getId());
        vo.setActive(entity.getActive());
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }
}
