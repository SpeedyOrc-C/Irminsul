<script lang="ts">
    export let title: string;
    export let content: string;
</script>

<div class="prompt">
    <div class="title font-hywh-85w">{@html title}</div>
    <div class="content font-hywh-65w">{@html content}</div>
</div>

<style>
    .prompt {
        position: absolute;
        top: 50%; left: 50%;
        transform: translate(-50%, -50%);
        
        width: 30rem;
        height: 20rem;
        padding: 2rem 3rem;
        border-radius: 0.5rem;

        background-color: #3e4457;
    }
    
    .title {
        margin-bottom: 1rem;
        
        text-align: center;
        font-size: 2rem;
        color: #d2bc8d;
    }

    .content {
        color: white;

        font-size: 1.2rem;
    }
</style>
