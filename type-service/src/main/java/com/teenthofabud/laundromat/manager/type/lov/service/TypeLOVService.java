package com.teenthofabud.laundromat.manager.type.lov.service;

import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVException;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVForm;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;

@Service
public interface TypeLOVService {

    public Set<TypeLOVVo> retrieveAllByNaturalOrdering();

    public TypeLOVVo retrieveDetailsById(long id) throws TypeLOVException;

    public List<TypeLOVVo> retrieveAllMatchingDetailsByName(String name) throws TypeLOVException;

    public Long createTypeLOV(TypeLOVForm form) throws TypeLOVException;

    public void updateTypeLOV(Long id, TypeLOVForm form) throws TypeLOVException;

    public void deleteTypeLOV(Long id) throws TypeLOVException;

    public void applyPatchOnTypeLOV(Long id, List<PatchOperationForm> patches) throws TypeLOVException;

}
