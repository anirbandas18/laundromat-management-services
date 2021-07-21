package com.teenthofabud.laundromat.manager.access.usertype.converter;

import com.teenthofabud.core.common.handler.TOABBaseEntityAuditHandler;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeEntity;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class UserTypeForm2EntityConverter extends TOABBaseEntityAuditHandler implements Converter<UserTypeForm, UserTypeEntity> {

    @Override
    public UserTypeEntity convert(UserTypeForm form) {
        UserTypeEntity entity = new UserTypeEntity();
        entity.setName(form.getName());
        entity.setDescription(form.getDescription());
        super.assignAuditValues(entity, Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }
}
