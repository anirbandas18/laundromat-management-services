package com.teenthofabud.laundromat.manager.type.model.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.model.error.TOABBaseException;
import com.teenthofabud.laundromat.manager.constant.TypeSubDomain;
import com.teenthofabud.laundromat.manager.error.TypeErrorCode;
import com.teenthofabud.laundromat.manager.error.TypeException;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVEntity;
import com.teenthofabud.laundromat.manager.type.lov.repository.TypeLOVRepository;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelDto;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Arrays;
import java.util.Collections;
import java.util.Optional;

@Component
@Slf4j
public class TypeModelDto2EntityConverter implements ComparativePatchConverter<TypeModelDto, TypeModelEntity> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 4;

    @Autowired
    public void setTypeLOVRepository(TypeLOVRepository typeLOVRepository) {
        this.typeLOVRepository = typeLOVRepository;
    }

    private TypeLOVRepository typeLOVRepository;


    @Override
    public void compareAndMap(TypeModelDto dto, TypeModelEntity actualEntity) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;
        Optional<String> optDescription = dto.getDescription();
        if(optDescription.isPresent()) {
            actualEntity.setDescription(optDescription.get());
            changeSW[i++] = true;
            log.debug("TypeModelDto.description is valid");
        }
        Optional<String> optName = dto.getName();
        if(optName.isPresent()) {
            actualEntity.setName(optName.get());
            changeSW[i++] = true;
            log.debug("TypeModelDto.name is valid");
        }
        Optional<String> optActive = dto.getActive();
        if(optActive.isPresent()) {
            actualEntity.setActive(Boolean.valueOf(optActive.get()));
            changeSW[i++] = true;
            log.debug("TypeModelDto.active is valid");
        }
        Optional<String> optTypeLovId = dto.getTypeLovId();
        if(optTypeLovId.isPresent()) {
            Long typeLovId = Long.parseLong(optTypeLovId.get());
            if(typeLOVRepository.existsById(typeLovId)) {
                Optional<TypeLOVEntity> optTypeLOV = typeLOVRepository.findById(typeLovId);
                if(optTypeLOV.isPresent()) {
                    actualEntity.setTypeLov(optTypeLOV.get());
                    changeSW[i++] = true;
                    log.debug("TypeModelDto.typeLovId is valid");
                }
                if(!changeSW[i == 0 ? i : i - 1]) {
                    throw new TypeException(TypeSubDomain.TYPE_MODEL, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "typeLovId" });
                }
            }
            if(!changeSW[i == 0 ? i : i - 1]) {
                throw new TypeException(TypeSubDomain.TYPE_MODEL, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "typeLovId" });
            }
        }
        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) >= 1) {
            log.debug("All provided TypeModelDto attributes are valid");
            actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided TypeModelDto attributes are valid");
    }
}
