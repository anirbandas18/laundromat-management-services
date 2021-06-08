package com.teenthofabud.core.common.dsa.lc;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentSkipListMap;

public class MinimumDeletionsUniqueCharacterFrequencies {

    public static void main(String[] args) throws IOException {
        BufferedReader br  = new BufferedReader(new InputStreamReader(System.in));
        String s = br.readLine();
        Map<Character, Integer> characterFrequencies = new HashMap<>();
        Map<Integer, List<Character>> adjustedCharacterFrequencies = new TreeMap<>();
        ConcurrentMap<Integer, Set<Character>> frequencyCharacters = new ConcurrentSkipListMap<>((i, j) -> {
            return Integer.compare(j, i);
        });
        for(int  i = 0 ; i < s.length() ; i++) {
            char c = s.charAt(i);
            characterFrequencies.put(c, characterFrequencies.getOrDefault(c, 0) + 1);
        }
        for(Character key : characterFrequencies.keySet()) {
            int frequency = characterFrequencies.get(key);
            Set<Character> value = frequencyCharacters.getOrDefault(frequency, new LinkedHashSet<Character>());
            value.add(key);
            frequencyCharacters.put(frequency, value);
        }
        System.out.println(characterFrequencies);
        System.out.println(frequencyCharacters);
        Integer deletionCount = 0;
        for(Integer frequencyCount : frequencyCharacters.keySet()) {
            List<Character> associatedCharacters = new LinkedList<>(frequencyCharacters.get(frequencyCount));
            if(associatedCharacters.size() > 1) {
                for(int i = 0 ; i < associatedCharacters.size() ; i++) {
                    Character c = associatedCharacters.get(i);
                    //Integer  charactersToBeDeleted = frequencyCount - (frequencyCount - i);
                    /*List<Character> updatedCharacterList = adjustedCharacterFrequencies.getOrDefault(charactersToBeDeleted, new LinkedList<Character>());
                    updatedCharacterList.add(c);
                    adjustedCharacterFrequencies.put(charactersToBeDeleted, updatedCharacterList);*/


                    Integer  charactersToBeDeleted = frequencyCount - (frequencyCount - i);
                    deletionCount = deletionCount + charactersToBeDeleted;
                    Integer  charactersToBeRetained = frequencyCount - i;
                    Set<Character> updatedCharacters = frequencyCharacters.getOrDefault(charactersToBeRetained, new LinkedHashSet<Character>());
                    updatedCharacters.add(c);
                    frequencyCharacters.put(charactersToBeRetained, updatedCharacters);
                }
            }
        }
        System.out.println(frequencyCharacters);
        /*System.out.println(adjustedCharacterFrequencies);
        for(Integer key : adjustedCharacterFrequencies.keySet()) {
            deletionCount = deletionCount + key;
        }*/
        System.out.println(deletionCount);
    }

}
