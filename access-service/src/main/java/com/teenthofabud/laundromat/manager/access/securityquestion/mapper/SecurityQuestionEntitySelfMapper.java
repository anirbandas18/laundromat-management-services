package com.teenthofabud.laundromat.manager.access.securityquestion.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.laundromat.manager.access.securityquestion.data.SecurityQuestionEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class SecurityQuestionEntitySelfMapper implements SingleChannelMapper<SecurityQuestionEntity> {
    @Override
    public Optional<SecurityQuestionEntity> compareAndMap(SecurityQuestionEntity source, SecurityQuestionEntity target) {
        SecurityQuestionEntity expectedEntity = new SecurityQuestionEntity();
        boolean changeSW = false;
        if(source.getId() != null && source.getId() > 0 && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source SecurityQuestionEntity.id is valid");
        }
        if(source.getName() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getName())) && source.getName().compareTo(target.getName()) != 0) {
            target.setName(source.getName());
            changeSW = true;
            log.debug("Source SecurityQuestionEntity.name is valid");
        }
        if(changeSW) {
            log.debug("All provided SecurityQuestionEntity attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided SecurityQuestionEntity attributes are valid");
            return Optional.empty();
        }
    }
}
