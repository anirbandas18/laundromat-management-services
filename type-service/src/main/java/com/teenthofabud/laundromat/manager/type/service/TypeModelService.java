package com.teenthofabud.laundromat.manager.type.service;

import com.teenthofabud.core.common.model.form.PatchOperationForm;
import com.teenthofabud.laundromat.manager.type.model.error.TypeException;
import com.teenthofabud.laundromat.manager.type.model.form.TypeModelForm;
import com.teenthofabud.laundromat.manager.type.model.vo.TypeModelVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;

@Service
public interface TypeModelService {

    public void init();

    public Set<TypeModelVo> retrieveAllByNaturalOrdering();

    public TypeModelVo retrieveDetailsById(long id) throws TypeException;
    public List<TypeModelVo> retrieveDetailsByTypeLOVId(long typeLovId) throws TypeException;

    public List<TypeModelVo> retrieveAllMatchingDetailsByName(String name) throws TypeException;

    public Long createTypeModel(TypeModelForm form) throws TypeException;

    public void updateTypeModel(Long id, TypeModelForm form) throws TypeException;

    public void deleteTypeModel(Long id) throws TypeException;

    public void applyPatchOnTypeModel(Long id, List<PatchOperationForm> patches) throws TypeException;



}
