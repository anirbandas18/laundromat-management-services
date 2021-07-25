package com.teenthofabud.laundromat.manager.access.securityquestion.service;

import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.laundromat.manager.access.securityquestion.data.SecurityQuestionException;
import com.teenthofabud.laundromat.manager.access.securityquestion.data.SecurityQuestionForm;
import com.teenthofabud.laundromat.manager.access.securityquestion.data.SecurityQuestionVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;

@Service
public interface SecurityQuestionService {

    public Set<SecurityQuestionVo> retrieveAllByNaturalOrdering();

    public SecurityQuestionVo retrieveDetailsById(long id) throws SecurityQuestionException;

    public List<SecurityQuestionVo> retrieveAllMatchingDetailsByName(String name) throws SecurityQuestionException;

    public Long createSecurityQuestion(SecurityQuestionForm form) throws SecurityQuestionException;

    public void updateSecurityQuestion(Long id, SecurityQuestionForm form) throws SecurityQuestionException;

    public void deleteSecurityQuestion(Long id) throws SecurityQuestionException;

    public void applyPatchOnSecurityQuestion(Long id, List<PatchOperationForm> patches) throws SecurityQuestionException;

}
