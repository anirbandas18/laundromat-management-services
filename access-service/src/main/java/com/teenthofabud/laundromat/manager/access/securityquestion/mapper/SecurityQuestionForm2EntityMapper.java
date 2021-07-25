package com.teenthofabud.laundromat.manager.access.securityquestion.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.laundromat.manager.access.securityquestion.data.SecurityQuestionEntity;
import com.teenthofabud.laundromat.manager.access.securityquestion.data.SecurityQuestionForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class SecurityQuestionForm2EntityMapper implements DualChannelMapper<SecurityQuestionEntity, SecurityQuestionForm> {
    @Override
    public Optional<SecurityQuestionEntity> compareAndMap(SecurityQuestionEntity actualEntity, SecurityQuestionForm form) {
        SecurityQuestionEntity expectedEntity = new SecurityQuestionEntity();
        boolean changeSW = false;
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying SecurityQuestionEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());
        expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
        log.debug("Directly copying SecurityQuestionEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
        expectedEntity.setActive(actualEntity.getActive());
        log.debug("Directly copying SecurityQuestionEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
        // comparative copy
        if(StringUtils.hasText(StringUtils.trimWhitespace(form.getName())) && form.getName().compareTo(actualEntity.getName()) != 0) {
            expectedEntity.setName(form.getName());
            changeSW = true;
            log.debug("SecurityQuestionForm.name: {} is different as SecurityQuestionEntity.name: {}", form.getName(), actualEntity.getName());
        } else {
            expectedEntity.setName(actualEntity.getName());
            log.debug("SecurityQuestionForm.name: is unchanged");
        }
        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }
}
