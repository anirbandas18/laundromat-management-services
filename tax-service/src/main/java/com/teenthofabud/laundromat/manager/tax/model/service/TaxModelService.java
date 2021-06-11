package com.teenthofabud.laundromat.manager.tax.model.service;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.laundromat.manager.tax.error.TaxException;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelForm;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;

@Service
public interface TaxModelService {

    public Set<TaxModelVo> retrieveAllByNaturalOrdering();

    public TaxModelVo retrieveDetailsById(Long id) throws TaxException;

    public List<TaxModelVo> retrieveDetailsByTaxTypeModelId(Long taxTypeModelId) throws TaxException;

    public List<TaxModelVo> retrieveDetailsByCurrencyTypeModelId(Long currencyTypeModelId) throws TaxException;

    public List<TaxModelVo> retrieveAllMatchingDetailsByName(String name) throws TaxException;

    public Long createTaxModel(TaxModelForm form) throws TaxException;

    public void updateTaxModel(Long id, TaxModelForm form) throws TaxException;

    public void deleteTaxModel(Long id) throws TaxException;

    public void applyPatchOnTaxModel(Long id, List<PatchOperationForm> patches) throws TOABBaseException;



}
