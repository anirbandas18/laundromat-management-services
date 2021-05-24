package com.teenthofabud.core.common.handler;

import com.teenthofabud.core.common.model.entity.TOABBaseEntity;
import lombok.extern.slf4j.Slf4j;

import java.time.LocalDateTime;
import java.time.ZoneOffset;

@Slf4j
public abstract class TOABBaseEntityConversionHandler {

    private static final Long DEFAULT_USER_ID = 1L;

    public void assignAuditValues(TOABBaseEntity entity, boolean useDefault) {
        entity.setCreatedOn(LocalDateTime.now(ZoneOffset.UTC));
        entity.setActive(Boolean.TRUE);
        entity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Setting audit attributes as {}", entity);
        if(useDefault) {
            entity.setCreatedBy(DEFAULT_USER_ID);
            entity.setModifiedBy(DEFAULT_USER_ID);
            log.debug("using default values for audit");
        }
    }

}
