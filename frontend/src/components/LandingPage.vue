<template>
    <body>
        <div class='container'>
            <div class='top'>
                <div class='logo'>
                    <span>
                        psf
                    </span>
                </div>
            </div>
            <div class='middle'>
                <h1 class='title'>
                    Open source quantitative trading 🍊
                </h1>
                <h2 class='link'>
                    <a href='https://github.com/pocketsizefund/pocketsizefund' target='_blank'>GitHub</a>
                </h2>
                <h2 class='link'>
                    <a href='https://twitter.com/pocketsizefund' target='_blank'>Twitter</a>
                </h2>
                <br />
                <br />
            </div>
        </div>
    </body>
</template>

<script>
export default {
    name: 'LandingPage',
};
</script>

<style>
body {
    font-family: 'Lexend', sans-serif;
    background-color: #ff9a00;
    height: 100%;
    width: 100%;
    color: #ffffff;
}

.container {
    height: 100%;
    margin: auto;
    width: 34rem;
    padding: 2rem 1rem 0rem;
}

.top>div {
    display: inline-block;
}

.logo {
    font-size: 3rem;
    font-weight: 800;
}

a {
    color: #ffffff;
}

.middle {
    padding-top: 6rem;
}

.title {
    font-size: 4rem;
    font-weight: 800;
    align-content: center;
    padding-bottom: 2rem;
}

.link {
    font-size: 3rem;
    font-weight: 800;
    display: inline;
    padding-right: 1rem;
    padding-bottom: 1rem;
}

@media screen and (max-width: 580px) {
    .container {
        width: 100%;
    }

    .middle {
        padding-top: 2rem;
    }

    .title {
        font-size: 2.5rem;
    }

    .link {
        font-size: 1.5rem;
    }
}
</style>