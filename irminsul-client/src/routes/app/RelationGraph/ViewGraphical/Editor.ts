import type {Vector2} from "$lib/util/Vector2";
import type {RelationGraph} from "../../../../model/RelationGraph";

export default class Editor {
    relationGraph: RelationGraph;

    private editMode = false;

    private selectedAtoms: Set<string> = new Set();
    private selectedClusters: Set<string> = new Set();

    private selectedEntities: Set<string> = new Set();
    private entitiesInSelectedClusters: Set<string> = new Set();

    private rootSelected = false;

    private entityAnchor: Map<string, Vector2> = new Map();
    private readonly rootAnchor: Vector2;

    constructor(relationGraph: RelationGraph) {
        this.relationGraph = relationGraph;
        this.rootAnchor = relationGraph.rootPosition;
        this.updateAnchors();
    }

    isEditing() {
        return this.editMode;
    }

    setEditing(editing: boolean) {
        this.editMode = editing;
        this.clearSelection();
        this.updateSelection();
    }

    isSelected(id: string) {
        return this.selectedEntities.has(id) || (id === this.relationGraph.id && this.rootSelected);
    }

    numSelected() {
        return this.selectedEntities.size + (this.rootSelected ? 1 : 0);
    }

    isEntityInSelectedCluster(id: string) {
        return this.entitiesInSelectedClusters.has(id);
    }

    toggleAtom(id: string) {
        if (this.editMode) {
            if (this.selectedAtoms.has(id)) {
                this.selectedAtoms.delete(id);
            } else {
                this.selectedAtoms.add(id);
            }
        } else {
            // In non-edit mode, only one atom can be selected at a time.
            if (this.numSelected() === 0) { // No atom selected
                this.selectedAtoms.add(id);

            } else if (this.numSelected() === 1) { // There is already one atom selected

                if (this.selectedAtoms.has(id)) { // The selected atom is clicked again
                    // Clear the selected atom
                    this.selectedAtoms.clear();

                } else { // Another atom is clicked
                    // Clear the previous selected atom and select the new one
                    this.selectedAtoms.clear();
                    this.selectedClusters.clear();
                    this.rootSelected = false;

                    this.selectedAtoms.add(id);
                }
            }
        }
        this.updateSelection();
    }

    toggleCluster(id: string) {
        if (this.editMode) {
            if (this.selectedClusters.has(id)) {
                this.selectedClusters.delete(id);
            } else {
                this.selectedClusters.add(id);
            }
        } else {
            // This part is similar to toggleAtom.
            // I just swapped selectedAtoms and selectedClusters.
            if (this.numSelected() === 0) {
                this.selectedClusters.add(id);

            } else if (this.numSelected() === 1) {

                if (this.selectedClusters.has(id)) {
                    this.selectedClusters.clear();

                } else {
                    this.selectedClusters.clear();
                    this.selectedAtoms.clear();
                    this.rootSelected = false;

                    this.selectedClusters.add(id);
                }
            }
        }
        this.updateSelection();
    }

    toggleRoot() {
        if (this.editMode) {
            this.rootSelected = !this.rootSelected;
        } else {
            // This part is similar to toggleAtom too.
            if (this.numSelected() === 0) {
                this.rootSelected = true;

            } else if (this.numSelected() === 1) {

                if (this.rootSelected) {
                    this.rootSelected = false;

                } else {
                    this.rootSelected = true;

                    this.selectedClusters.clear();
                    this.selectedAtoms.clear();
                }
            }
        }
        this.updateSelection();
    }

    moveSelectedElements(delta: Vector2) {
        this.relationGraph.atoms
            .filter(a => this.selectedAtoms.has(a.id))
            .forEach(a => {
                a.position.x += delta.x;
                a.position.y += delta.y;
            });

        this.relationGraph.clusters
            .filter(c => this.selectedClusters.has(c.id))
            .forEach(c => {
                c.position.x += delta.x;
                c.position.y += delta.y;
                // Anchor is relative to the cluster position, move it too.
                c.anchor.x += delta.x;
                c.anchor.y += delta.y;
            });

        if (this.rootSelected) {
            this.rootAnchor.x += delta.x;
            this.rootAnchor.y += delta.y;
        }

        this.updateAnchors();
    }

    moveSelectedClustersAnchors(delta: Vector2) {
        this.relationGraph.clusters
            .filter(c => this.selectedClusters.has(c.id))
            .forEach(c => {
                c.anchor.x += delta.x;
                c.anchor.y += delta.y;
            });

        this.updateAnchors();
    }

    selectAll() {
        this.clearSelection();
        this.relationGraph.atoms.forEach(a => this.selectedAtoms.add(a.id));
        this.relationGraph.clusters.forEach(c => this.selectedClusters.add(c.id));
        this.rootSelected = true;
        this.updateSelection();
    }

    deselectAll() {
        this.clearSelection();
        this.updateSelection();
    }

    reverseSelect() {
        this.relationGraph.atoms.forEach(a => {
            if (this.selectedAtoms.has(a.id)) {
                this.selectedAtoms.delete(a.id);
            } else {
                this.selectedAtoms.add(a.id);
            }
        });
        this.relationGraph.clusters.forEach(c => {
            if (this.selectedClusters.has(c.id)) {
                this.selectedClusters.delete(c.id);
            } else {
                this.selectedClusters.add(c.id);
            }
        });
        this.rootSelected = !this.rootSelected;
        this.updateSelection();
    }

    anchorOf(id: string) {
        return this.entityAnchor.get(id) ?? {x: 0, y: 0};
    }

    positionOf(id: string) {
        return this.relationGraph.atoms.find(a => a.id === id)?.position
            ?? this.relationGraph.clusters.find(c => c.id === id)?.position
            ?? {x: 0, y: 0};
    }

    private clearSelection() {
        this.selectedAtoms.clear();
        this.selectedClusters.clear();
        this.rootSelected = false;
    }

    private updateSelection() {
        this.selectedEntities.clear();
        for (const a of this.selectedAtoms) this.selectedEntities.add(a)
        for (const c of this.selectedClusters) this.selectedEntities.add(c);

        this.entitiesInSelectedClusters.clear();
        this.relationGraph.clusters
            .filter(c => this.selectedClusters.has(c.id))
            .flatMap(c => c.elements)
            .forEach(e => this.entitiesInSelectedClusters.add(e));
    }

    private updateAnchors() {
        this.entityAnchor.clear();

        this.relationGraph.atoms.forEach(a => this.entityAnchor.set(a.id, a.position));
        this.relationGraph.clusters.forEach(c => this.entityAnchor.set(c.id, c.anchor));
        this.entityAnchor.set(this.relationGraph.id, this.rootAnchor);
    }
}
