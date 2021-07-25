package com.teenthofabud.laundromat.manager.access.securityquestion.converter;

import com.teenthofabud.core.common.handler.TOABBaseEntityAuditHandler;
import com.teenthofabud.laundromat.manager.access.securityquestion.data.SecurityQuestionEntity;
import com.teenthofabud.laundromat.manager.access.securityquestion.data.SecurityQuestionForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class SecurityQuestionForm2EntityConverter extends TOABBaseEntityAuditHandler implements Converter<SecurityQuestionForm, SecurityQuestionEntity> {

    @Override
    public SecurityQuestionEntity convert(SecurityQuestionForm form) {
        SecurityQuestionEntity entity = new SecurityQuestionEntity();
        entity.setName(form.getName());
        super.assignAuditValues(entity, Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }
}
